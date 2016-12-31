{-# LANGUAGE TemplateHaskell #-}

module DBtest where

import qualified DB as D
import MyArbitrary

import qualified Database.HDBC as HS
import Data.Maybe (isJust)

import Data.ByteString
import Data.Time.Clock
import qualified Data.Set as Set

-- testing
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_SetupWorks = monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        D.setup db
        r <- D.verify db
        D.disconnect db
        return r
    assert v

prop_getVersion = monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        D.setup db
        v <- D.getVersion db
        D.disconnect db
        return v
    assert $ v == D.version

prop_selectModTime_empty path time = monadicIO $ do
    v <- run $ do
        db <- D.connect ":memory:"
        D.setup db
        c <- D.sqlSelectModtime db
        HS.execute c [HS.SqlString $! path, HS.SqlUTCTime $! time]
        m <- HS.fetchRow c
        v <- return . (==False). isJust $ m
        D.disconnect db
        return v
    assert v
        
{-
-- TODO add fake datasets
prop_insertFiles dataset = monadicIO $ do
    v <- run $ do
        files <- arbitrary `suchThat` nodups -- `suchThat` ((>0) . Prelude.length)
        db <- D.connect ":memory:"
        D.setup db
        --insertFiles db dataset
        --v <- return . (==False). isJust $ m
        D.disconnect db
        return True
    assert v

nodups :: Ord a => [a] -> Bool
nodups = nodups' Set.empty where
  nodups' _ [] = True
  nodups' a (b : c) = not (Set.member b a) && nodups' (Set.insert b a) c


-}

return []
runTests = $quickCheckAll
