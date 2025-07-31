{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Game where

import Bluefin.Eff (Eff, (:>))
import Bluefin.State
import Bluefin.StateSource (newState, withStateSource)
import Broadcast
import Color (assignColor)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (sortBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Strict.Map qualified as Map
import Data.Traversable (for)
import Datastar qualified
import Html qualified
import Lucid hiding (for_)
import Queue (Queue, tryWriteQueue)
import Random
import RawSse (RawEvent (..))
import Sleep
import Store
import Types

data NewPlayerStatus
    = NoNewPlayer
    | AlreadyInGame
    | GameFull
    | AddedTemporarly User

advanceState
    :: GameState -> Maybe (Int, Int) -> SneksDirections -> Eff es (SneksDirections, GameState)
advanceState gameState mbNewFood sneksDirectionsBefore = withStateSource \source -> do
    foodEaten <- newState source Set.empty
    murderList <- newState source []
    -- Add new snek, if he survives this round they can stay.
    (aliveSneks, sneksDirections, newPlayerStatus) <- case gameState.newPlayer of
        Nothing -> do
            pure (gameState.aliveSneks, sneksDirectionsBefore, NoNewPlayer)
        Just (newSnek, newSnekDirection) -> do
            let newSneks = newSnek : gameState.aliveSneks
            let newSneksDirections =
                    Map.insert newSnek.user.userId newSnekDirection sneksDirectionsBefore
            if length gameState.aliveSneks >= gameState.maxPlayers
                then pure (gameState.aliveSneks, sneksDirectionsBefore, GameFull)
                else
                    if newSnek.user `elem` map (.user) gameState.aliveSneks
                        then pure (gameState.aliveSneks, sneksDirectionsBefore, AlreadyInGame)
                        else pure (newSneks, newSneksDirections, AddedTemporarly newSnek.user)
    -- Calculate new positions and eliminate some.
    movedSneks <-
        catMaybes <$> for aliveSneks \snek -> do
            case Map.lookup snek.user.userId sneksDirections of
                -- Something went wrong, clearly, let's put the poor thing out of its misery.
                Nothing -> do
                    modify murderList (snek.user.userId :) -- :)
                    pure Nothing
                Just snekDirection -> do
                    let realDirection = fromMaybe snekDirection.current snekDirection.new
                    let (c, r) = snek.headOfSnek
                    let newHeadOfSnek@(newC, newR) = case realDirection of
                            U -> (c, r - 1)
                            D -> (c, r + 1)
                            L -> (c - 1, r)
                            R -> (c + 1, r)
                    let ateFood = newHeadOfSnek `elem` gameState.foodPositions
                    let newRestOfSnek =
                            if ateFood
                                then snek.headOfSnek : snek.restOfSnek
                                else init (snek.headOfSnek : snek.restOfSnek)
                    when ateFood do
                        modify foodEaten (Set.insert newHeadOfSnek)
                    if newR > gameState.boardSize
                        || newR < 0
                        || newC > gameState.boardSize
                        || newC < 0
                        then do
                            modify murderList (snek.user.userId :) -- :)
                            pure Nothing
                        else
                            pure $
                                Just $
                                    MkSnek
                                        snek.user
                                        snek.color
                                        newHeadOfSnek
                                        newRestOfSnek
                                        (max 0 (snek.grace - 1))
    -- Eliminate some more.
    allHeads <- for movedSneks \snek -> do
        pure (snek.user.userId, snek.headOfSnek, snek.grace)
    allTaken <-
        concat <$> for movedSneks \snek ->
            pure $
                (snek.user.userId, True, snek.headOfSnek, snek.grace)
                    : map (\x -> (snek.user.userId, False, x, snek.grace)) snek.restOfSnek
    for_ allTaken \(someUserId, someIsHead, taken, someGracePeriod) -> do
        let killCandidates =
                map (\(otherUserId, _, _) -> otherUserId)
                    . filter
                        ( \(otherUserId, otherHead, otherGracePeriod) ->
                            ( someUserId /= otherUserId
                                || (gameState.snekSelfOwn && not someIsHead && someUserId == otherUserId)
                            )
                                && taken == otherHead
                                && otherGracePeriod == 0
                                && someGracePeriod == 0
                        )
                    $ allHeads
        for_ killCandidates \poorThing -> do
            modify murderList (poorThing :) -- :)
    toMurder <- get murderList
    -- Actually do the killing.
    let newSneksDirections =
            Map.map
                ( \snekDirection ->
                    MkSnekDirection
                        { current =
                            fromMaybe snekDirection.current snekDirection.new
                        , new = Nothing
                        }
                )
                -- Learn to swim.
                . Map.withoutKeys sneksDirections
                . Set.fromList
                $ toMurder
    let newSneks =
            filter (\snek -> snek.user.userId `notElem` toMurder) movedSneks

    -- Food management.
    eaten <- get foodEaten
    let foodNotEaten =
            Set.filter (\(c, r) -> c <= gameState.boardSize || r <= gameState.boardSize)
                . Set.difference gameState.foodPositions
                $ eaten
    let newGameState =
            gameState
                { aliveSneks = newSneks
                , foodPositions = case mbNewFood of
                    Just newFood -> Set.insert newFood foodNotEaten
                    Nothing -> foodNotEaten
                , newPlayer = case newPlayerStatus of
                    NoNewPlayer -> Nothing
                    AlreadyInGame -> Nothing
                    GameFull -> gameState.newPlayer
                    AddedTemporarly newUserId ->
                        if newUserId `elem` map (.user) newSneks
                            then Nothing -- They managed to get into the game.
                            else gameState.newPlayer -- We will try again.
                }
    pure (newSneksDirections, newGameState)

run
    :: ( e1 :> es
       , e2 :> es
       , e3 :> es
       , e4 :> es
       , e5 :> es
       , e6 :> es
       )
    => Random e1
    -> StoreWrite e2
    -> StoreRead e3
    -> BroadcastServer StoreUpdate e4
    -> Queue () e5
    -> Sleep e6
    -> Eff es ()
run random storeWrite storeRead broadcastGameStateServer mainPageQueue sleep =
    evalState [] \allTimeBestS -> do
        evalState [] \currentBestS -> do
            foreverWithSleep sleep 200 do
                foodPositions <- getFoodPositions storeRead
                boardSize <- (.boardSize) <$> getSettings storeRead
                maxFood <- (.maxFood) <$> getSettings storeRead
                maybeNewFood <- maybeSpawnFood random boardSize (Set.size foodPositions) maxFood
                maxPlayers <- (.maxPlayers) <$> getSettings storeRead
                gracePeriod <- (.gracePeriod) <$> getSettings storeRead
                mbNewPlayer <-
                    getNewPlayer storeRead >>= \case
                        Nothing -> pure Nothing
                        Just user -> Just <$> randomSnekAndDirection random gracePeriod user boardSize
                gameState <-
                    MkGameState boardSize foodPositions
                        <$> getSneks storeRead
                        <*> pure mbNewPlayer
                        <*> pure maxPlayers
                        <*> ((.snekSelfOwn) <$> getSettings storeRead)
                (newSneksDirections, newGameState) <-
                    -- This way we won't lose move commands.
                    atomicModifySnekDirection storeWrite (advanceState gameState maybeNewFood)

                case (mbNewPlayer, newGameState.newPlayer) of
                    -- There was a new player, and we successfully added it to the game.
                    (Just _, Nothing) -> putNewPlayer storeWrite Nothing
                    _ -> pure ()
                renderWebComponent <- (.useWebComponent) <$> getSettings storeRead
                anonymousMode <- (.anonymousMode) <$> getSettings storeRead
                calculateLeaderboard allTimeBestS currentBestS newGameState.aliveSneks
                allTimeBest <- get allTimeBestS
                currentBest <- get currentBestS
                settings <- getSettings storeRead
                isQueueFull <- getIsQueueFull storeRead
                let settingsHtml =
                        Html.settingsGrid
                            (length newGameState.foodPositions)
                            isQueueFull
                            (length newGameState.aliveSneks)
                            settings
                let leaderboardHtml = Html.leaderboard anonymousMode currentBest allTimeBest
                let (event, frame) =
                        render
                            settingsHtml
                            leaderboardHtml
                            isQueueFull
                            anonymousMode
                            newGameState
                            newSneksDirections
                            renderWebComponent
                putSneks storeWrite newGameState.aliveSneks
                putFoodPositions storeWrite newGameState.foodPositions
                putGameFrame storeWrite frame
                writeBroadcast broadcastGameStateServer event
                _ <- tryWriteQueue mainPageQueue ()
                (.frameTimeMs) <$> getSettings storeRead

maybeSpawnFood :: (e :> es) => Random e -> Int -> Int -> Int -> Eff es (Maybe (Int, Int))
maybeSpawnFood random boardSize currentFood maxFood =
    if maxFood <= 0 || currentFood >= maxFood
        then pure Nothing
        else do
            let spareCapacity = maxFood - currentFood
            let spawnProbability = (fromIntegral spareCapacity / fromIntegral maxFood) ^ (2 :: Int)
            r <- random01 random
            if r < spawnProbability
                then Just <$> randomIn random boardSize boardSize
                else pure Nothing

render
    :: Html ()
    -> Html ()
    -> Bool
    -> Bool
    -> GameState
    -> SneksDirections
    -> Bool
    -> (StoreUpdate, RawEvent)
render settingsHtml leaderboardHtml isQueueFull anonymousMode gameState sneksDirections = \cases
    False -> do
        let gameFrame =
                Datastar.patchElements
                    . Html.renderFrame isQueueFull settingsHtml leaderboardHtml
                    $ Html.renderBoard
                        anonymousMode
                        (MkUser "" "")
                        gameState
        -- This is not very efficient.
        let userGameFrame =
                Map.fromList
                    . map
                        ( \snek ->
                            ( snek.user.userId
                            , Datastar.patchElements
                                . Html.renderFrame isQueueFull settingsHtml leaderboardHtml
                                $ Html.renderBoard
                                    anonymousMode
                                    snek.user
                                    gameState
                            )
                        )
                    $ gameState.aliveSneks
        (GameFrameUpdate userGameFrame gameFrame sneksDirections, gameFrame)
    True -> do
        let webComponent =
                Html.renderFrame isQueueFull settingsHtml leaderboardHtml $
                    Html.renderBoardWebComponent anonymousMode gameState
        let rawEvent = Datastar.patchElements webComponent
        (WebComponentUpdate rawEvent sneksDirections, rawEvent)

randomSnekAndDirection
    :: (e :> es)
    => Random e
    -> Int
    -> User
    -> Int
    -> Eff es (Snek, SnekDirection)
randomSnekAndDirection random gracePeriod user maxSize = do
    let dirOffsets =
            (U, (0, -1))
                NonEmpty.:| [ (D, (0, 1))
                            , (L, (-1, 0))
                            , (R, (1, 0))
                            ]
    (dir, (dc, dr)) <- randomFromList random dirOffsets
    (c, r) <-
        -- All those +2 to make sure we don't spawn too close to the edges.
        (\(c', r') -> (c' + 2, r' + 2))
            <$> randomIn random (maxSize - 2) (maxSize - 2)
    let headPos = (c, r)
    let tailPos = (c + dc, r + dr)
    let snek =
            MkSnek
                { user = user
                , color = assignColor (userIdToText user.userId)
                , headOfSnek = headPos
                , restOfSnek = [tailPos]
                , grace = gracePeriod
                }
    let snekDir =
            MkSnekDirection
                { current = dir
                , new = Nothing
                }
    pure (snek, snekDir)

updateScoreboard :: Int -> Sneks -> Sneks -> Maybe Sneks
updateScoreboard maxEntries currentSneks newSneks = do
    let allSneks = currentSneks ++ newSneks
    let longestPerUser =
            Map.elems $
                Map.fromListWith
                    maxByLength
                    [(snek.user.userId, snek) | snek <- allSneks]
    let sortedSneks = take maxEntries $ sortBy compareSneks longestPerUser
    if currentSneks /= sortedSneks
        then Just sortedSneks
        else Nothing
  where
    maxByLength s1 s2 = if snekLength s1 >= snekLength s2 then s1 else s2

    compareSneks s1 s2 = case comparing (negate . snekLength) s1 s2 of
        EQ -> comparing (.user.userId) s1 s2
        ord -> ord

snekLength :: Snek -> Int
snekLength snek = 1 + length snek.restOfSnek -- +1 for head.

calculateLeaderboard
    :: (e1 :> es, e2 :> es)
    => State [Snek] e1
    -> State [Snek] e2
    -> Sneks
    -> Eff es ()
calculateLeaderboard allTimeBestS currentBestS sneks = do
    allTimeBest <- get allTimeBestS
    currentBest <- get currentBestS
    let currentBestAlive = filter (`elem` sneks) currentBest
    let mbNewAllTimeBest = updateScoreboard 5 allTimeBest sneks
    let mbCurrentBestS = updateScoreboard 10 currentBestAlive sneks
    let newAllTimeBest = fromMaybe allTimeBest mbNewAllTimeBest
    let newCurrentBest = fromMaybe currentBestAlive mbCurrentBestS
    put allTimeBestS newAllTimeBest
    put currentBestS newCurrentBest
