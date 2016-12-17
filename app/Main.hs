module Main where

import qualified DHT as D
import Control.Concurrent
import Control.Monad

import Logger
import FSWatcher

logModule = rootLogger

sleepTime = 20000000

test dht = do
    id <- D.generateID
    debugM logModule $ show id
    t <- D.search dht id
    threadDelay sleepTime
    D.nodes >>= debugM logModule . show

main :: IO ()
main = do
    setupLogger DEBUG

    id <- D.generateID
    infoM logModule "starting DHT"
    dht <- D.startDHT (Just 0) Nothing 4445 id
    infoM logModule "started DHT"
    n <- D.bootstrap "nodes.dump"

    infoM logModule $ "found "++show n++" nodes"
    infoM logModule "bootstrap nodes sent"

    forkIO testFS

    test dht
    test dht
    test dht

    threadDelay sleepTime
    D.saveBootstrap "nodes.dump"
    D.stopDHT dht

    closeLogger
    infoM logModule $ "Closing"

