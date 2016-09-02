module Main where

import qualified DHT as D
import Control.Concurrent.MVar
import Control.Concurrent

main :: IO ()
main = do
    putStrLn "starting"
    id <- D.generateID
    barrier <- newEmptyMVar
    D.withDHT (Just 0) Nothing 4444 id "" $ \dht -> do
        putStrLn "started DHT"
        t <- takeMVar barrier
        putStrLn ".."
        return ()
    D.addRemoteBootstrapNodes
    putStrLn "bootstrap nodes sent"
    threadDelay 20000000
    t <- D.nodes
    print t
    putMVar barrier ()

