module Main where

import qualified DHT as D
import Control.Concurrent.MVar
import Control.Concurrent

main :: IO ()
main = do
    putStrLn "starting"
    id <- D.generateID
    dht <- D.startDHT (Just 0) Nothing 4444 id ""
    putStrLn "started DHT"
    D.addRemoteBootstrapNodes
    putStrLn "bootstrap nodes sent"
    threadDelay 20000000
    t <- D.nodes
    print t
    id <- D.generateID
    D.search dht id
    threadDelay 20000000
    t <- D.nodes
    print t
    D.stopDHT dht
