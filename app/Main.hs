module Main where

import qualified DHT as D

main :: IO ()
main = do
    D.stopDHT
    putStrLn "moo"
