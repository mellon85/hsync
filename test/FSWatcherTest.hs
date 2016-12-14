{-# LANGUAGE TemplateHaskell #-}

module FSWatcherTest where

import Data.Conduit
import Data.Conduit.List
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

prop_SimpleDiff_Eq path1 path2 = path1 == path2 ==> monadicIO $ do
    v <- run $ do
        t <- getCurrentTime
        s1 <- return $ sourceList [fromPath path1 t]
        s2 <- return $ sourceList [fromPath path2 t]
        df <- FS.findDiffs s1 s2 $$ consume
        return $ length df
    assert $ v == 0

return []
runTests = $quickCheckAll
