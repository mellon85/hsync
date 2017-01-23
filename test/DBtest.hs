{-# LANGUAGE TemplateHaskell #-}

module DBtest where

import qualified DB as D
import MyArbitrary

import qualified Database.HDBC as HS
import Data.Maybe (isJust, fromJust)
import Data.List (sortBy)

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

prop_insertFile entry =
    nodups entry ==> monadicIO $ do
    (files,entries) <- run $ do
        db <- D.connect ":memory:"
        D.insertFile db entry
        files <- runResourceT . C.sourceToList . D.allPaths $ db
        entrydb <- mapM (D.getEntry db) files
        D.disconnect db
        return $ (files, map fromJust entrydb)
    assert $ (length $ filter (not . isError) entries) == length files
    assert $ entriesDBcheck entry files
    assert $ entries == (sortPath $ filter (not . isError) entry)

sortPath = sortBy comparison
    where
        comparison a b = compare (entryPath a) (entryPath b)

prop_upsertFile entry =
    nodups entry ==> monadicIO $ do
    (f1, f2, e1, e2) <- run $ do
        db <- D.connect ":memory:"
        D.upsertFile db entry
        files1 <- runResourceT . C.sourceToList . D.allPaths $ db
        entry1 <- mapM (D.getEntry db) files1
        D.upsertFile db entry
        files2 <- runResourceT . C.sourceToList . D.allPaths $ db
        entry2 <- mapM (D.getEntry db) files2
        D.disconnect db
        return (files1, files2, map fromJust entry1, map fromJust entry2)
    assert $ f1 == f2
    assert $ entriesDBcheck entry f1
    assert $ (length f1) == (length $ filter (not . isError) entry)
    assert $ e1 == (sortPath $ filter (not . isError) entry)
    assert $ e1 == e2

prop_getEntry entry = (not . isError) entry ==> monadicIO $ do
    (Just v) <- run $ do
        db <- D.connect ":memory:"
        D.insertFile db [entry]
        D.getEntry db $ entryPath entry
    assert $ v == entry

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
