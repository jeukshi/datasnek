module BotManager (run) where

import Bluefin.Coroutine
import Bluefin.Eff
import Bluefin.State
import Bluefin.StateSource (newState, withStateSource)
import Broadcast
import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (minimumBy)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Ord
import Data.Strict.Map qualified as Map
import Data.Strict.Set (Set)
import Data.Strict.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import GenUuid
import Random
import Store
import Types

run
    :: (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es)
    => StoreRead e1
    -> BroadcastClient StoreUpdate e2
    -> BroadcastServer Command e3
    -> Random e4
    -> GenUuid e5
    -> Eff es ()
run storeRead broadcastStore broadcastCommand random genUuid = do
    withStateSource \source -> do
        snekBotsS <- newState source []
        snekBotInQueueS <- newState source Nothing
        forEach (likeAndSubscribe broadcastStore) \case
            GameFrameUpdate{} -> do
                manageBots storeRead broadcastCommand random genUuid snekBotsS snekBotInQueueS
            WebComponentUpdate _ _ -> do
                manageBots storeRead broadcastCommand random genUuid snekBotsS snekBotInQueueS
            _ -> pure ()

manageBots
    :: (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es)
    => StoreRead e1
    -> BroadcastServer Command e2
    -> Random e3
    -> GenUuid e4
    -> State [UserId] e5
    -> State (Maybe UserId) e6
    -> Eff es ()
manageBots storeRead broadcastCommand random genUuid snekBotsS snekBotInQueueS = do
    aliveSneks <- getSneks storeRead
    snekBots <- get snekBotsS
    let aliveSneksIds = map ((.user.userId)) aliveSneks
    let aliveSnekBots =
            filter (`elem` aliveSneksIds) snekBots
    put snekBotsS aliveSnekBots
    maxBots <- (.maxBots) <$> getSettings storeRead
    foodPositions <- getFoodPositions storeRead
    boardSize <- (.boardSize) <$> getSettings storeRead
    get snekBotInQueueS >>= \case
        Nothing -> pure ()
        Just userId -> when (userId `elem` aliveSneksIds) do
            -- Our bot managed to get into the game.
            put snekBotInQueueS Nothing
            modify snekBotsS (userId :)
    tryReadSneksDirections storeRead >>= \case
        Just directions -> do
            let snekBotWithDirection =
                    map
                        ( \snek ->
                            ( snek
                            , (.current)
                                . Map.findWithDefault (MkSnekDirection U Nothing) snek.user.userId
                                $ directions
                            )
                        )
                        . filter (\snek -> snek.user.userId `elem` aliveSnekBots)
                        $ aliveSneks
            updatedDirections <-
                catMaybes <$> updateBotDirections random foodPositions boardSize snekBotWithDirection
            for_ updatedDirections \(userId, direction) ->
                writeBroadcast broadcastCommand (ChangeDirection userId direction)
            get snekBotInQueueS >>= \case
                Just _ -> pure ()
                Nothing -> when (length aliveSneksIds < maxBots) do
                    botId <- userIdFromUuid <$> nextUuid genUuid
                    botName <- randomFromList random unsuspiciousListOfRandomNames
                    botEmoji <- randomFromList random emoji
                    let botUser = MkUser (botName <> "-bot " <> botEmoji) botId
                    put snekBotInQueueS (Just botId)
                    writeBroadcast broadcastCommand (PlayRequest botUser)
        Nothing -> pure () -- Strange if it isn't there, but not much can be done about it.

updateBotDirections
    :: (e :> es)
    => Random e
    -> Set (Int, Int)
    -> Int
    -> [(Snek, Direction)]
    -> Eff es [Maybe (UserId, Direction)]
updateBotDirections random foodPositions boardSize snekWithDirection = do
    for snekWithDirection \(snek, currentDirection) -> do
        newDirection <- decideBotDirection random foodPositions boardSize snek currentDirection
        if newDirection == currentDirection
            then pure Nothing
            else pure $ Just (snek.user.userId, newDirection)

decideBotDirection
    :: (e :> es)
    => Random e
    -> Set (Int, Int)
    -> Int
    -> Snek
    -> Direction
    -> Eff es Direction
decideBotDirection random foodPositions boardSize snek currentSnekDir = do
    between01 <- random01 random
    if between01 < 0.2
        then randomFromList random (getPossibleMoves snek boardSize currentSnekDir)
        else
            makeFoodSeekingMove
                random
                foodPositions
                snek
                (getPossibleMoves snek boardSize currentSnekDir)

getPossibleMoves :: Snek -> Int -> Direction -> NonEmpty Direction
getPossibleMoves snek maxDim currentDir = do
    let (headC, headR) = snek.headOfSnek
    let allPossibleDirections = [U, D, L, R]
    let oppositeDir = getOppositeDirection currentDir
    let nonOppositeMoves = filter (/= oppositeDir) allPossibleDirections
    let validMoves =
            filter
                ( \dir ->
                    let (newC, newR) = case dir of
                            U -> (headC, headR - 1)
                            D -> (headC, headR + 1)
                            L -> (headC - 1, headR)
                            R -> (headC + 1, headR)
                     in newC >= 0 && newC <= maxDim && newR >= 0 && newR <= maxDim
                )
                nonOppositeMoves
    case validMoves of
        [] -> currentDir NonEmpty.:| []
        (x : xs) -> x NonEmpty.:| xs

getOppositeDirection :: Direction -> Direction
getOppositeDirection U = D
getOppositeDirection D = U
getOppositeDirection L = R
getOppositeDirection R = L

makeFoodSeekingMove
    :: (e :> es)
    => Random e
    -> Set (Int, Int)
    -> Snek
    -> NonEmpty Direction
    -> Eff es Direction
makeFoodSeekingMove random foodPositions snek possibleMoves = do
    if null foodPositions
        then randomFromList random possibleMoves
        else do
            let closestFood = minimumBy (comparing (manhattanDistance snek.headOfSnek)) (Set.toList foodPositions)
            let currentHead = snek.headOfSnek
            let movesTowardsFood =
                    NonEmpty.filter (movesBringCloserToFood currentHead closestFood) possibleMoves
            case movesTowardsFood of
                [] -> randomFromList random possibleMoves
                (x : xs) -> randomFromList random (x NonEmpty.:| xs)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

movesBringCloserToFood :: (Int, Int) -> (Int, Int) -> Direction -> Bool
movesBringCloserToFood (headC, headR) foodPos dir = do
    let newPos = case dir of
            U -> (headC, headR - 1)
            D -> (headC, headR + 1)
            L -> (headC - 1, headR)
            R -> (headC + 1, headR)
    let currentDistance = manhattanDistance (headC, headR) foodPos
    let newDistance = manhattanDistance newPos foodPos
    newDistance < currentDistance

unsuspiciousListOfRandomNames :: NonEmpty Text
unsuspiciousListOfRandomNames =
    -- Completely random list of names for our bots.
    -- Taeyeon is first, accidentaly, but I have to say: slay queen!
    "Taeyeon"
        NonEmpty.:| [ "IU"
                    , "BoA"
                    , "Sunmi"
                    , "Chungha"
                    , "Hyuna"
                    , "Somi"
                    , "Heize"
                    , "Yuqi"
                    , "Rosé"
                    , "Jennie"
                    , "Yoona"
                    , "Tiffany"
                    , "Jessica"
                    , "Krystal"
                    , "Suzy"
                    , "ain"
                    , "Lee"
                    , "Ailee"
                    , "Luna"
                    , "Lisa"
                    , "Jisoo"
                    , "Nayeon"
                    , "Sana"
                    , "Momo"
                    , "Jihyo"
                    , "Mina"
                    , "Dahyun"
                    , "Chaeyoung"
                    , "Tzuyu"
                    , "Jeongyeon"
                    , "Irene"
                    , "Seulgi"
                    , "Wendy"
                    , "Joy"
                    , "Yeri"
                    , "Miyeon"
                    , "Minnie"
                    , "Soojin"
                    , "Soyeon"
                    , "Shuhua"
                    , "Karina"
                    , "Winter"
                    , "iselle"
                    , "Ningning"
                    , "Wonyoung"
                    , "Yujin"
                    , "Rei"
                    , "aeul"
                    , "Liz"
                    , "Leeseo"
                    , "Minji"
                    , "Hanni"
                    , "Danielle"
                    , "Haerin"
                    , "Hyein"
                    , "Sakura"
                    , "Chaewon"
                    , "Yunjin"
                    , "Kazuha"
                    , "Eunchae"
                    , "Yuna"
                    , "Yeji"
                    , "Lia"
                    , "Ryujin"
                    , "Chaeryeong"
                    , "Sunny"
                    , "Tiffany"
                    , "Hyoyeon"
                    , "Yuri"
                    , "Sooyoung"
                    , "Seohyun"
                    , "Victoria"
                    , "Amber"
                    , "Sulli"
                    , "JeA"
                    , "Miryo"
                    , "Narsha"
                    , "Nicole"
                    , "Yuri"
                    , "Seungyeon"
                    , "Hara"
                    , "Jiyoung"
                    , "CL"
                    , "Bom"
                    , "Dara"
                    , "Minzy"
                    , "Jiyeon"
                    , "Hyomin"
                    , "Eunjung"
                    , "Qri"
                    , "Boram"
                    , "Soyeon"
                    , "Hwayoung"
                    , "Chorong"
                    , "Bomi"
                    , "Eunji"
                    , "Naeun"
                    , "Namjoo"
                    , "Hayoung"
                    ]

emoji :: NonEmpty Text
emoji =
    "♡⃛"
        NonEmpty.:| [ "☆彡"
                    , "♪♫"
                    , "◕‿◕"
                    , "(*´∀`)"
                    , "◦❀◦"
                    , "✧･ﾟ"
                    , "(◡‿◡)"
                    , "ﾟ･✧"
                    , "♡´･ᴗ･`♡"
                    , "(˘▾˘)"
                    , "☆ミ"
                    , "♫♪"
                    , "◕ ◡ ◕"
                    , "(*^▽^*)"
                    , "❀◦"
                    , "✦･ﾟ"
                    , "(´｡• ω •｡`)"
                    , "☆*:"
                    , "♡(˃͈ દ ˂͈ ༶ )"
                    , "(>‿◠)✌"
                    , "♪～"
                    , "◦✧"
                    , "(´∩｡• ᵕ •｡∩`)"
                    , "☆:."
                    , "♡♡"
                    , "(*ﾟ▽ﾟ*)"
                    , "◦♫"
                    , "✧*｡"
                    , "(◕‿‿◕)"
                    ]
