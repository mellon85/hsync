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

-- @TODO remove noErrors property and check correctly
prop_insertFile entry =
    noErrors entry ==>
    nodups entry ==> monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        D.insertFile db entry
        files <- runResourceT . C.sourceToList . D.allPaths $ db
        D.disconnect db
        return $ length files
        -- @TODO retrieve all and check data is identical
    assert $ length entry == v

-- @TODO remove noErrors property and check correctly
prop_upsertFile entry =
    noErrors entry ==>
    nodups entry ==> monadicIO $ do
    (f1,f2) <- run $ do
        db <- D.connect ":memory:"
        D.upsertFile db entry
        files1 <- runResourceT . C.sourceToList . D.allPaths $ db
        -- check that data can be overwriten without error
        D.upsertFile db entry
        files2 <- runResourceT . C.sourceToList . D.allPaths $ db
        D.disconnect db
        return (length files1, length files2)
        -- @TODO retrieve all and check data is identical
    assert $ f1 == f2 && f1 == (length entry)

nodups :: [Entry] -> Bool
nodups = nodups' Set.empty Set.empty
    where
    nodups' _ _ [] = True
    nodups' a p (b : c) = not (Set.member b a)
                     && not (Set.member (entryPath b) p)
                     && nodups' (Set.insert b a) (Set.insert (entryPath b) p) c

noErrors :: [Entry] -> Bool
noErrors = all (not . isError)

return []
runTests = $quickCheckAll
