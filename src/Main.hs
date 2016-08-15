{-
module Main where

import FSWatcher
import Logger
import System.IO

mimi :: LoggerT IO Int
mimi = do
    logError "1"
    logError "2"
    logError "3"
    logError "4"
    logScope "scope" mimi2
    return 0

mimi2 = do
    logError "r"
    logError "r"
    return 0

main_ = do
    mimi
    mimi2
    mimi

main = do
    p <- runLoggerT (INFO, "test.log") main_
    return p

-}
