module Main where

import qualified DHT as D

main :: IO ()
main = do
    id <- D.generateID
    D.runDHT (Just 0) Nothing 4444 id ""
    putStrLn "moo"
