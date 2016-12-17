{-# LANGUAGE TemplateHaskell #-}

module FSWatcherTest where

import Data.Conduit
import Data.Conduit.List hiding(head, take, drop)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers

import Data.Time.Clock

import qualified FSWatcher as FS
import MyArbitrary

fromPath p t = FS.File p t False

allDiff [] = True
allDiff [_] = True
allDiff (a:xs) | a /= (head xs) &&
                 FS.entryPath a /= FS.entryPath (head xs) = allDiff xs
               | otherwise      = False

prop_findDiffs_correct (Ordered ex) (Positive a) (Positive b) = allDiff ex &&
            a >= 0 && a < b && b < length ex ==> monadicIO $ do
    (v,t) <- run $ do
        ex1 <- return $ take a ex
        ex2 <- return $ take (a-b) $ drop a ex
        common <- return $ drop b ex
        s1 <- return $ sourceList $ ex1 ++ common
        s2 <- return $ sourceList $ ex2 ++ common
        df <- FS.findDiffs s1 s2 $$ consume
        return $ (length df, length ex1+length ex2)
    assert $ v == t

return []
runTests = $quickCheckAll
