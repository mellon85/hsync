module Main where

import qualified DHT as D
import Control.Concurrent
import Control.Monad

import Logger
import FSWatcher

logModule = rootLogger

test dht = do
    id <- D.generateID
    debugM logModule $ show id
    t <- D.search dht id
    threadDelay 20000000
    D.nodes >>= debugM logModule . show

main :: IO ()
main = do
    setupLogger DEBUG

    id <- D.generateID
    dht <- D.startDHT (Just 0) Nothing 4445 id
    infoM logModule "started DHT"
    n <- D.conditionalBootstrap "nodes.dump"

    infoM logModule $ "found "++show n++" nodes"
    infoM logModule "bootstrap nodes sent"

    forkIO testFS

    test dht
    test dht
    test dht

    threadDelay 200000000
    D.saveBootstrap "nodes.dump"
    D.stopDHT dht

    closeLogger

