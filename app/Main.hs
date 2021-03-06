module Main where

import qualified Discover.DHT as D
import qualified Discover.Broadcast as B
import Control.Concurrent
import Control.Monad

import Configuration as C

import Logger
import FSWatcher

logModule = "APP"

sleepTime = 20000000

test dht = do
    id <- D.generateID
    debugM logModule $ show id
    t <- D.search dht id
    threadDelay sleepTime
    D.nodes >>= debugM logModule . show

info = infoM logModule
debug = debugM logModule

main :: IO ()
main = do
    --conf <- return $ C.readDefaultConfiguration
    conf <- readDefaultConfiguration

    -- setup logger level
    setupLogger . loggingLevel . logging $ conf
    debug $ show conf

    id <- D.generateID
    info "starting DHT"
    dht <- D.start (Just 0) Nothing 4445 id
    info "started DHT"
    n <- D.bootstrap "nodes.dump"

    info $ "found "++show n++" nodes"
    info "bootstrap nodes sent"

    -- start broadcast
    b <- startBroadcast $ broadcast conf

    forkIO testFS

    test dht
    test dht
    test dht

    threadDelay 200000000
    maybe (return ()) (B.stop) b

    D.saveBootstrap "nodes.dump"
    D.stop dht

    closeLogger
    infoM logModule $ "Closing"

startBroadcast :: C.BroadcastConf -> IO (Maybe B.Broadcast)
startBroadcast conf | broadcastEnabledV4 conf ||  broadcastEnabledV6 conf = do
                        b <- B.start (fromIntegral $ broadcastPort conf)
                                     (broadcastEnabledV4 conf)
                                     (broadcastEnabledV6 conf)
                                     (fromIntegral $ broadcastInterval conf)
                        return . Just $ b
                    | otherwise = return Nothing

