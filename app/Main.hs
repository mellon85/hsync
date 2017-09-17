module Main where

import qualified Discover.DHT as D
import qualified Discover.Broadcast as B
import Control.Concurrent
import Control.Monad

import Configuration as C

import DB
import Logger
import FSWatcher

import Data.Conduit
import qualified Data.Conduit.List as CL

logModule = "APP"

test dht = do
    id <- D.generateID
    debugM logModule $ show id
    t <- D.search dht id
    threadDelay 20000000
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

startBroadcast :: C.BroadcastConf -> IO (Maybe B.Broadcast)
startBroadcast conf | broadcastEnabled conf = do
                        b <- B.start (fromIntegral $ broadcastPort conf) (fromIntegral $ broadcastInterval conf)
                        return . Just $ b
                    | otherwise = return Nothing



testFS = do
    c <- DB.connect "test.db"
    b <- DB.verify c
    unless b $ error "db corrupted"
    -- should not use filterInfo, hashing can change an Info to an Error
    hashConduit "." c $$ CL.mapM_ (debugM logModule . show)
    DB.disconnect c
