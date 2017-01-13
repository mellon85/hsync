{-# LANGUAGE TemplateHaskell #-}

module DBtest where

import qualified DB as D
import MyArbitrary

import qualified Database.HDBC as HS
import Data.Maybe (isJust)

import Data.ByteString
import FileEntry
import Data.Time.Clock
import qualified Data.Set as Set

-- testing
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_SetupWorks = monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        r <- D.verify db
        D.disconnect db
        return r
    assert v

prop_getVersion = monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        v <- D.getVersion db
        D.disconnect db
        return v
    assert $ v == D.version

prop_isFileNewer_empty path time = monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        c <- D.isFileNewer db path time
        v <- return $ c == False
        D.disconnect db
        return v
    assert v

prop_insertFile entry = monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        D.insertFile db entry
        D.disconnect db
        return True
    assert v

prop_insertFiles files = nodups files ==> monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        mapM_ (D.insertFile db) files
        D.disconnect db
        return True
    assert v

nodups :: [Entry] -> Bool
nodups = nodups' Set.empty Set.empty
    where
    nodups' _ _ [] = True
    nodups' a p (b : c) = not (Set.member b a)
                     && not (Set.member (entryPath b) p)
                     && nodups' (Set.insert b a) (Set.insert (entryPath b) p) c

return []
runTests = $quickCheckAll
