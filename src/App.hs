module App (run) where

import Api
import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Eff (runEff)
import Bluefin.Extra (accessConcurrently, growScope)
import Bluefin.IO (effIO)
import Bluefin.Once (runOnce)
import Bluefin.Proxy (captureEffEnv)
import BotManager qualified
import Broadcast (runBroadcast)
import ChatManager qualified
import CommandManager qualified
import Css qualified
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Favicon qualified
import Game qualified
import GenUuid (runGenUuid)
import Network.Wai.Handler.Warp qualified as Warp
import Queue
import QueueManager qualified
import Random (runRandom)
import Servant (Application)
import Servant.Server.Generic (genericServeTWithContext)
import Sleep
import Store
import Types
import WebComponents qualified

run :: Env -> IO ()
run env = runEff \io -> do
    adminPassword <- case env of
        Prod -> UUID.toASCIIBytes <$> effIO io do UUID.nextRandom
        Dev -> pure "admin1"
    effIO io do print adminPassword
    -- Bluefin pyramid of doom.
    BC.runSTM io \stme -> do
        runStore io stme env \storeReadMain storeWriteMain storeChatReadMain -> do
            runBroadcast io storeReadMain \broadcastGameStateClientMain broadcastGameStateServerMain -> do
                runBroadcast io storeReadMain \broadcastCommandClientMain broadcastCommandServerMain -> do
                    runRandom io \randomMain -> do
                        runGenUuid io \genUuidMain -> do
                            runQueue stme 5 \gameQueueMain -> do
                                runQueue stme 100 \chatQueueMain -> do
                                    runQueue stme 2 \mainPageQueueMain -> do
                                        BC.withNonDet io \nonDet -> do
                                            BC.scoped nonDet \scope -> do
                                                _ <- BC.fork scope $ \acc -> do
                                                    broadcastGameStateServer <- accessConcurrently acc broadcastGameStateServerMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    chatQueue <- accessConcurrently acc chatQueueMain
                                                    mainPageQueue <- accessConcurrently acc mainPageQueueMain
                                                    ChatManager.run storeWrite storeRead chatQueue broadcastGameStateServer mainPageQueue

                                                _ <- BC.fork scope $ \acc -> do
                                                    random <- accessConcurrently acc randomMain
                                                    broadcastGameStateClient <- accessConcurrently acc broadcastGameStateClientMain
                                                    broadcastCommandServer <- accessConcurrently acc broadcastCommandServerMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    genUuid <- accessConcurrently acc genUuidMain
                                                    BotManager.run storeRead broadcastGameStateClient broadcastCommandServer random genUuid

                                                _ <- BC.fork scope $ \acc -> do
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    gameQueue <- accessConcurrently acc gameQueueMain
                                                    localIO <- accessConcurrently acc io
                                                    runSleep localIO \sleep -> do
                                                        QueueManager.run storeWrite storeRead gameQueue sleep

                                                _ <- BC.fork scope $ \acc -> do
                                                    broadcastCommandClient <- accessConcurrently acc broadcastCommandClientMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    gameQueue <- accessConcurrently acc gameQueueMain
                                                    chatQueue <- accessConcurrently acc chatQueueMain
                                                    localScope <- accessConcurrently acc scope
                                                    growScope localScope \newScope -> do
                                                        CommandManager.run
                                                            storeWrite
                                                            storeRead
                                                            gameQueue
                                                            chatQueue
                                                            newScope
                                                            broadcastCommandClient

                                                _ <- BC.fork scope $ \acc -> do
                                                    random <- accessConcurrently acc randomMain
                                                    broadcastGameStateServer <- accessConcurrently acc broadcastGameStateServerMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    mainPageQueue <- accessConcurrently acc mainPageQueueMain
                                                    localIO <- accessConcurrently acc io
                                                    runSleep localIO \sleep -> do
                                                        Game.run
                                                            random
                                                            storeWrite
                                                            storeRead
                                                            broadcastGameStateServer
                                                            mainPageQueue
                                                            sleep

                                                warpThread <- BC.fork scope $ \acc -> do
                                                    ioLocal <- accessConcurrently acc io
                                                    broadcastGameStateClient <- accessConcurrently acc broadcastGameStateClientMain
                                                    broadcastCommandServer <- accessConcurrently acc broadcastCommandServerMain
                                                    genUuid <- accessConcurrently acc genUuidMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    storeChatRead <- accessConcurrently acc storeChatReadMain
                                                    broadcastGameStateServer <- accessConcurrently acc broadcastGameStateServerMain
                                                    mainPageQueue <- accessConcurrently acc mainPageQueueMain
                                                    runOnce ioLocal \once -> do
                                                        effEnvProxy <- captureEffEnv
                                                        let routes =
                                                                Routes
                                                                    { _page = page storeRead
                                                                    , _loginPage = loginPage storeRead
                                                                    , _transmittal =
                                                                        transmittal
                                                                            storeChatRead
                                                                            broadcastGameStateClient
                                                                    , _hotreload = hotreload once
                                                                    , _play = play storeRead broadcastCommandServer
                                                                    , _chat = chat broadcastCommandServer
                                                                    , _changeDirection = changeDirection broadcastCommandServer
                                                                    , _login = login genUuid
                                                                    , _css = pure Css.file
                                                                    , _favicon = pure Favicon.file
                                                                    , _snekcomponent = pure WebComponents.snekGameControllerJS
                                                                    , _windowController = pure WebComponents.windowControllerJS
                                                                    , _settings =
                                                                        settings
                                                                            storeWrite
                                                                            storeRead
                                                                            broadcastGameStateServer
                                                                            mainPageQueue
                                                                    }
                                                        let app :: Application = genericServeTWithContext (nt ioLocal effEnvProxy) routes (ctx adminPassword)
                                                        effIO ioLocal do Warp.run 3000 app
                                                BC.awaitEff warpThread
