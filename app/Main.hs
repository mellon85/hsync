module Main where

import qualified DHT as D
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
    putStrLn "starting"
    id <- D.generateID
    dht <- D.startDHT (Just 0) Nothing 4445 id
    putStrLn "started DHT"
    D.conditionalBootstrap "nodes.dump"
    putStrLn "bootstrap nodes sent"

    id <- D.generateID
    t <- D.search dht id
    threadDelay 20000000
    D.nodes >>= print

    id <- D.generateID
    t <- D.search dht id
    threadDelay 20000000
    D.nodes >>= print


    id <- D.generateID
    t <- D.search dht id
    threadDelay 20000000
    D.nodes >>= print

    threadDelay 200000000
    D.saveBootstrap "nodes.dump"
    D.stopDHT dht

