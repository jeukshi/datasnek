module BotManager (run) where

import Bluefin.Coroutine
import Bluefin.Eff
import Bluefin.IO (effIO)
import Bluefin.Internal qualified
import Bluefin.State
import Bluefin.StateSource (newState, withStateSource)
import Broadcast
import Command
import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (minimumBy)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Ord
import Data.Strict.Map qualified as Map
import Data.Strict.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import GenUuid
import Random
import Snek
import Store
import StoreUpdate
import User

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
            GameFrameUpdate _ _ sneksDirections -> do
                manageBots storeRead broadcastCommand random genUuid snekBotsS snekBotInQueueS sneksDirections
            WebComponentUpdate _ sneksDirections -> do
                manageBots storeRead broadcastCommand random genUuid snekBotsS snekBotInQueueS sneksDirections
            _ -> pure ()

manageBots
    :: (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es)
    => StoreRead e1
    -> BroadcastServer Command e2
    -> Random e3
    -> GenUuid e4
    -> State [UserId] e5
    -> State (Maybe UserId) e6
    -> SneksDirections
    -> Eff es ()
manageBots storeRead broadcastCommand random genUuid snekBotsS snekBotInQueueS snekDirection = do
    aliveSneks <- getSneks storeRead
    snekBots <- get snekBotsS
    let aliveSneksIds = map ((.user.userId)) aliveSneks
    let aliveSnekBots =
            filter (`elem` aliveSneksIds) snekBots
    put snekBotsS aliveSnekBots
    maxBots <- getMaxBots storeRead
    foodPositions <- getFoodPositions storeRead
    boardSize <- getBoardSize storeRead
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
    let possibleMoves = getPossibleMoves snek boardSize currentSnekDir
    if between01 < 0.03
        then randomFromList random possibleMoves
        else makeFoodSeekingMove random foodPositions snek currentSnekDir possibleMoves

getPossibleMoves :: Snek -> Int -> Direction -> NonEmpty Direction
getPossibleMoves snek max currentDir = do
    let (row, col) = snek.headOfSnek
    let allMoves = [U, D, L, R]
    let oppositeDir = getOppositeDirection currentDir
    let validMoves = filter (/= oppositeDir) allMoves
    if null validMoves
        then currentDir NonEmpty.:| []
        else head validMoves NonEmpty.:| tail validMoves

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
    -> Direction
    -> NonEmpty Direction
    -> Eff es Direction
makeFoodSeekingMove random foodPositions snek currentSnekDir possibleMoves = do
    let closestFood = minimumBy (comparing (manhattanDistance snek.headOfSnek)) foodPositions
    let movesTowardsFood =
            NonEmpty.filter (movesBringCloserToFood snek.headOfSnek closestFood) possibleMoves
    let moves =
            if null movesTowardsFood
                then currentSnekDir NonEmpty.:| []
                else head movesTowardsFood NonEmpty.:| tail movesTowardsFood
    randomFromList random moves

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

movesBringCloserToFood :: (Int, Int) -> (Int, Int) -> Direction -> Bool
movesBringCloserToFood (row, col) foodPos dir = do
    let newPos = case dir of
            U -> (row - 1, col)
            D -> (row + 1, col)
            L -> (row, col - 1)
            R -> (row, col + 1)
    let currentDistance = manhattanDistance (row, col) foodPos
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
