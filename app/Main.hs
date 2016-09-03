module Main where

import qualified DHT as D
import Control.Concurrent.MVar
import Control.Concurrent

main :: IO ()
main = do
    putStrLn "starting"
    id <- D.generateID
    dht <- D.startDHT (Just 0) Nothing 4444 id "nodes.dump"
    putStrLn "started DHT"
    --D.addRemoteBootstrapNodes
    putStrLn "bootstrap nodes sent"
    threadDelay 20000000
    D.nodes >>= print
    putStrLn "Stopping"
    D.nodes >>= print
    D.stopDHT dht
    threadDelay 20000000
