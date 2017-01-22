{-# LANGUAGE TemplateHaskell #-}

module DBtest where

import qualified DB as D
import MyArbitrary

import qualified Database.HDBC as HS
import Data.Maybe (isJust)

import qualified Data.ByteString as BS
import FileEntry
import Data.Time.Clock
import qualified Data.Set as Set
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)

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

-- @TODO should check that hashes and symlink and whatnot data is correct
prop_insertFile entry =
    nodups entry ==> monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        D.insertFile db entry
        files <- runResourceT . C.sourceToList . D.allPaths $ db
        D.disconnect db
        return $ files
    assert $ (length $ filter (not . isError) entry) == length v
    assert $ entriesDBcheck entry v

-- @TODO should check that hashes and symlink and whatnot data is correct
prop_upsertFile entry =
    nodups entry ==> monadicIO $ do
    (f1,f2) <- run $ do
        db <- D.connect ":memory:"
        D.upsertFile db entry
        files1 <- runResourceT . C.sourceToList . D.allPaths $ db
        D.upsertFile db entry
        files2 <- runResourceT . C.sourceToList . D.allPaths $ db
        D.disconnect db
        return (files1, files2)
    assert $ f1 == f2
    assert $ entriesDBcheck entry f1
    assert $ (length f1) == (length $ filter (not . isError) entry)


entry2set :: [Entry] -> Set.Set String
entry2set = Set.fromList . map entryPath

entriesDBcheck entries files = diff == 0
    where
        diff = Set.size (Set.difference files' entries')
        files' = Set.fromList files
        entries' = entry2set entries

-- | Property to verify that there are no duplicated entries
nodups :: [Entry] -> Bool
nodups = nodups' Set.empty Set.empty
    where
    nodups' _ _ [] = True
    nodups' a p (b : c) = not (Set.member b a)
                     && not (Set.member (entryPath b) p)
                     && nodups' (Set.insert b a) (Set.insert (entryPath b) p) c

-- | Property to verify that there are no errors in the generated Entries
noErrors :: [Entry] -> Bool
noErrors = all (not . isError)

return []
runTests = $quickCheckAll
