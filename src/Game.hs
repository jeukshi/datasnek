{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Game where

import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Eff (Eff, (:>))
import Bluefin.Extra (ThreadSafe (accessConcurrently))
import Bluefin.IO (effIO)
import Bluefin.Internal qualified
import Bluefin.State
import Bluefin.StateSource (newState, withStateSource)
import Bluefin.Stream (forEach)
import Broadcast
import Color (assignColor)
import Command
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM qualified as STM
import Control.Monad (forever, when)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Graph (components)
import Data.List (partition, sortBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Strict.Map (Map)
import Data.Strict.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Traversable (for)
import Lucid hiding (for_)
import Lucid.Datastar (dataAttr_)
import Message
import Queue
import Random
import RawSse (RawEvent (..))
import RenderHtml qualified
import Servant (FromHttpApiData (..))
import Sleep
import Snek
import Store
import StoreUpdate
import Unsafe.Coerce (unsafeCoerce)
import User
import WebComponents

data GameState = MkGameState
    { boardSize :: Int
    , foodPositions :: Set (Int, Int)
    , aliveSneks :: Sneks
    , newPlayer :: Maybe (Snek, SnekDirection)
    , maxPlayers :: Int
    }
    deriving (Show)

data NewPlayerStatus
    = NoNewPlayer
    | AlreadyInGame
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
                        else pure $ Just $ MkSnek snek.user snek.color newHeadOfSnek newRestOfSnek
    -- Eliminate some more.
    allHeads <- for movedSneks \snek -> do
        pure (snek.user.userId, snek.headOfSnek)
    allTaken <-
        concat <$> for movedSneks \snek -> do
            pure $ map (\x -> (snek.user.userId, x)) (snek.headOfSnek : snek.restOfSnek)
    for_ allTaken \(someUserId, taken) -> do
        let killCandidates =
                map fst
                    . filter
                        (\(otherUserId, otherHead) -> someUserId /= otherUserId && taken == otherHead)
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
       , e7 :> es
       , e8 :> es
       , e9 :> es
       , e4 :> scopeEs
       )
    => Random e1
    -> StoreWrite e2
    -> StoreRead e3
    -> Queue User e4
    -> Queue (User, Message) e4
    -> BC.Scope scopeEs e5
    -> BroadcastClient Command e6
    -> BroadcastServer StoreUpdate e7
    -> Queue () e8
    -> Sleep e9
    -> Eff es ()
run random storeWrite storeRead gameQueue chatQueue scope broadcastCommandClient broadcastGameStateServer mainPageQueue sleep =
    evalState [] \allTimeBestS -> do
        evalState [] \currentBestS -> do
            foreverWithSleep sleep 200 do
                boardSize <- getBoardSize storeRead
                foodPositions <- getFoodPositions storeRead
                maxFood <- getMaxFood storeRead
                maybeNewFood <- maybeSpawnFood random boardSize (Set.size foodPositions) maxFood
                maxPlayers <- getMaxPlayers storeRead
                mbNewPlayer <-
                    getNewPlayer storeRead >>= \case
                        Nothing -> pure Nothing
                        Just user -> Just <$> randomSnekAndDirection random user boardSize
                gameState <-
                    MkGameState boardSize foodPositions
                        <$> getSneks storeRead
                        <*> pure mbNewPlayer
                        <*> pure maxPlayers
                (newSneksDirections, newGameState) <-
                    -- This way we won't lose move commands.
                    atomicModifySnekDirection storeWrite (advanceState gameState maybeNewFood)

                case (mbNewPlayer, newGameState.newPlayer) of
                    -- There was a new player, and we successfully added it to the game.
                    (Just _, Nothing) -> putNewPlayer storeWrite Nothing
                    _ -> pure ()
                renderWebComponent <- getRenderWebComponent storeRead
                anonymousMode <- getAnonymousMode storeRead
                calculateLeaderboard allTimeBestS currentBestS newGameState.aliveSneks
                allTimeBest <- get allTimeBestS
                currentBest <- get currentBestS
                let leaderboardHtml = RenderHtml.leaderboard anonymousMode currentBest allTimeBest
                putLeaderboardHtml storeWrite leaderboardHtml
                let (event, frame) =
                        render leaderboardHtml anonymousMode newGameState newSneksDirections renderWebComponent
                -- FIXME name the thing
                putSneks storeWrite newGameState.aliveSneks
                putFoodPositions storeWrite newGameState.foodPositions
                putGameFrame storeWrite frame
                writeBroadcast broadcastGameStateServer event
                _ <- tryWriteQueue mainPageQueue ()
                getGameFrameTimeMs storeRead

maybeSpawnFood :: (e :> es) => Random e -> Int -> Int -> Int -> Eff es (Maybe (Int, Int))
maybeSpawnFood random boardSize currentFood maxFood =
    if maxFood <= 0 || currentFood >= maxFood
        then pure Nothing
        else do
            let spareCapacity = maxFood - currentFood
            let spawnProbability = (fromIntegral spareCapacity / fromIntegral maxFood) ^ 2
            r <- random01 random
            if r < spawnProbability
                then Just <$> randomIn random boardSize boardSize
                else pure Nothing

render :: Html () -> Bool -> GameState -> SneksDirections -> Bool -> (StoreUpdate, RawEvent)
render leaderboardHtml anonymousMode gameState sneksDirections = \cases
    False -> do
        let gameFrame =
                renderBoardToRawEvent
                    leaderboardHtml
                    anonymousMode
                    (MkUser "" "")
                    gameState
        -- This is not very efficient.
        let userGameFrame =
                Map.fromList
                    . map
                        ( \snek ->
                            ( snek.user.userId
                            , renderBoardToRawEvent
                                leaderboardHtml
                                anonymousMode
                                snek.user
                                gameState
                            )
                        )
                    $ gameState.aliveSneks
        (GameFrameUpdate userGameFrame gameFrame sneksDirections, gameFrame)
    True -> do
        let webComponent = renderWebComponentToRawEvent leaderboardHtml anonymousMode gameState
        (WebComponentUpdate webComponent sneksDirections, webComponent)

renderBoardToRawEvent :: Html () -> Bool -> User -> GameState -> RawEvent
renderBoardToRawEvent leaderboardHtml anonymousMode user gameState =
    MkRawEvent $
        "event:datastar-patch-elements\n"
            <> "data:elements "
            <> renderBoard leaderboardHtml anonymousMode user gameState
            <> "\n"

renderBoard :: Html () -> Bool -> User -> GameState -> BL.ByteString
renderBoard leaderboardHtml anonymousMode renderForUser gameState = renderBS do
    div_ [id_ "game-area", class_ "game-area"] do
        leaderboardHtml
        div_ [id_ "board", class_ "board"] $
            sequence_
                [ div_ [class_ "board-container"] do
                    sequence_
                        [ case Map.lookup (c, r) snakeCells of
                            (Just (user, color, isHead)) -> div_ [class_ (base <> " snake"), style_ ("background-color:" <> color)] do
                                let username = if anonymousMode then "snek" else toHtml user.name
                                if isHead
                                    then
                                        if renderForUser.userId == user.userId
                                            then div_ [class_ "nameplate-me"] username
                                            else div_ [class_ "nameplate"] username
                                    else mempty
                            Nothing -> do
                                let cls =
                                        if (c, r) `elem` gameState.foodPositions
                                            then base <> " food"
                                            else base
                                div_ [class_ cls] mempty
                        | c <- [0 .. gameState.boardSize]
                        ]
                | r <- [0 .. gameState.boardSize]
                ]
  where
    snakeCells :: Map (Int, Int) (User, Text, Bool)
    snakeCells =
        Map.fromList $
            [(coord, (s.user, s.color, False)) | s <- gameState.aliveSneks, coord <- s.restOfSnek]
                -- It is important that heads go last into the list.
                -- That way head will be always in the map,
                -- so we can display nameplate properly.
                ++ [(s.headOfSnek, (s.user, s.color, True)) | s <- gameState.aliveSneks]
    base = "board-item"

randomSnekAndDirection
    :: (e :> es)
    => Random e
    -> User
    -> Int
    -> Eff es (Snek, SnekDirection)
randomSnekAndDirection random user maxSize = do
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
                }
    let snekDir =
            MkSnekDirection
                { current = dir
                , new = Nothing
                }
    pure (snek, snekDir)

renderWebComponentToRawEvent :: Html () -> Bool -> GameState -> RawEvent
renderWebComponentToRawEvent leaderboardHtml anonymousMode gameState =
    MkRawEvent $
        "event:datastar-patch-elements\n"
            <> "data:elements "
            <> renderWebComponent leaderboardHtml anonymousMode gameState
            <> "\n"

encodeToText :: Aeson.Value -> T.Text
encodeToText = T.decodeUtf8 . BL.toStrict . Aeson.encode

renderWebComponent :: Html () -> Bool -> GameState -> BL.ByteString
renderWebComponent leaderboardHtml anonymousMode gameState = renderBS $ do
    let foodJson = encodeToText . Aeson.toJSON . Set.toList $ gameState.foodPositions
    let snakesJson = encodeToText . Aeson.toJSON $ map snekToObject gameState.aliveSneks
    let boardSizeTxt = toText gameState.boardSize
    div_ [id_ "game-area", class_ "game-area"] do
        leaderboardHtml
        div_ [id_ "board", class_ "board"] do
            snekGameBoard_
                [ id_ "snek-game-board"
                , boardSize_ boardSizeTxt
                , food_ foodJson
                , sneks_ snakesJson
                , dataAttr_ "username" "$username"
                , anonymous_ anonymousMode
                ]
                mempty

toText :: (Show a) => a -> Text
toText = T.pack . show

snekToObject :: Snek -> Aeson.Value
snekToObject (MkSnek (MkUser name userId) color (hx, hy) rest) =
    Aeson.object
        [ "username" Aeson..= Aeson.String name
        , "color" Aeson..= Aeson.String color
        , "headOfSnek" Aeson..= Aeson.toJSON (hx, hy)
        , "restOfSnek" Aeson..= Aeson.toJSON rest
        ]

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

leaderboardToRawEvent :: Html () -> RawEvent
leaderboardToRawEvent html =
    MkRawEvent $
        "event:datastar-patch-elements\n"
            <> "data:elements "
            <> renderBS html
            <> "\n"
