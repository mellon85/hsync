{-# LANGUAGE TemplateHaskell #-}

module FSWatcherTest where

import Data.Conduit
import Data.Conduit.List hiding(head, take, drop)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Time.Clock

import qualified FSWatcher as FS
import MyArbitrary

fromPath p t = FS.File p t False

prop_SimpleDiff path1 path2 = path1 /= path2 ==> monadicIO $ do
    v <- run $ do
        t <- getCurrentTime
        s1 <- return $ sourceList [fromPath path1 t]
        s2 <- return $ sourceList [fromPath path2 t]
        df <- FS.findDiffs s1 s2 $$ consume
        return $ length df
    assert $ v == 2

prop_SimpleDiff_Eq e1 = monadicIO $ do
    v <- run $ do
        t <- getCurrentTime
        s1 <- return $ sourceList e1
        s2 <- return $ sourceList e1
        df <- FS.findDiffs s1 s2 $$ consume
        return $ length df
    assert $ v == 0

allDiff [] = True
allDiff [_] = True
allDiff (a:xs) | a /= (head xs) &&
                 FS.entryPath a /= FS.entryPath (head xs) = allDiff xs
               | otherwise      = False

prop_diff ex = allDiff ex ==> monadicIO $ do
    v <- run $ do
        s1 <- return $ sourceList $ take 1 ex
        s2 <- return $ sourceList $ drop 1 ex
        df <- FS.findDiffs s1 s2 $$ consume
        return $ length df
    assert $ v == length ex

return []
runTests = $quickCheckAll
