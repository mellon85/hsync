{-# LANGUAGE TemplateHaskell #-}

module FSWatcherTest where

import Data.Conduit
import Data.Conduit.List hiding(head, take, drop)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers

import Data.List (sort)
import Data.Time.Clock

import qualified FSWatcher as FS
import MyArbitrary

fromPath p t = FS.File p t False

allDiff [] = True
allDiff [_] = True
allDiff (a:xs) | a /= (head xs) &&
                 FS.entryPath a /= FS.entryPath (head xs) = allDiff xs
               | otherwise      = False

prop_findDiffs ex1 ex2 common =
        allDiff (sort (ex1++ex2++common)) ==> monadicIO $ do
    (v,t) <- run $ do
        s1 <- return . sourceList . sort $ ex1 ++ common
        s2 <- return . sourceList . sort $ ex2 ++ common
        df <- FS.findDiffs s2 s1 $$ consume
        return $ (length df, length ex1+length ex2)
    assert $ v == t

return []
runTests = $quickCheckAll
