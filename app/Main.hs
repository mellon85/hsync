module Main where

import qualified Discover.DHT as D
import qualified Discover.Broadcast as B
import Control.Concurrent
import Control.Monad

import Configuration as C

import Logger
import FSWatcher
import OS.Signals
import Control.Concurrent.MVar

logModule = "APP"

test dht = do
    id <- D.generateID
    debugM logModule $ show id
    t <- D.search dht id
    threadDelay 20000000
    D.nodes >>= debugM logModule . show

info = infoM logModule

main :: IO ()
main = do
    -- rest of the application is covered by the signal handler
    v <- newEmptyMVar
    installSignalHandlers (do
        info $ "Termination request received"
        putMVar v ())

    --conf <- return $ C.readDefaultConfiguration
    conf <- return $ C.defaultConfig

    -- setup logger level
    setupLogger $ C.loggerLevel conf

    id <- D.generateID
    info "starting DHT"
    dht <- D.start (Just 0) Nothing 4445 id
    info "started DHT"
    n <- D.bootstrap "nodes.dump"

    info $ "found "++show n++" nodes"
    info "bootstrap nodes sent"

    -- start broadcast
    b <- startBroadcast conf

    forkIO testFS
    takeMVar v

    maybe (return ()) (B.stop) b
    D.saveBootstrap "nodes.dump"
    D.stop dht
    closeLogger

startBroadcast :: C.Configuration -> IO (Maybe B.Broadcast)
startBroadcast conf | broadcastEnabled conf = do
                        b <- B.start (fromInteger . toInteger $ broadcastPort conf) (fromInteger . toInteger $ broadcastRefresh conf)
                        return . Just $ b
                    | otherwise = return Nothing

