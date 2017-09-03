module MyArbitrary where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Word (Word8)
import qualified Database.HDBC as HS
import Data.Time.Clock
import qualified Data.ByteString as BS
import Data.Time.Calendar
import qualified FSWatcher as FS
import qualified HashUtils as HU
import Control.Exception.Base

instance Arbitrary Day where
    arbitrary = do
        day <- getNonNegative <$> arbitrary
        return $ ModifiedJulianDay day
    shrink = map ModifiedJulianDay . shrinkIntegral . toModifiedJulianDay

instance Arbitrary DiffTime where
    arbitrary = secondsToDiffTime <$> choose (0, 86401)
    shrink = map picosecondsToDiffTime . shrinkIntegral . ceiling . toRational

instance Arbitrary UTCTime where
    arbitrary = do
        day <- arbitrary
        difftime <- arbitrary
        return $ UTCTime day difftime

instance Arbitrary HU.ChunkedSum where
    arbitrary = do
        checksum <- arbitrary
        return $ HU.CS checksum (checksum `mod` HU.max_size)

instance Arbitrary FS.Entry where
    arbitrary = do
        which <- choose (0,4) :: Gen Int
        t <- arbitrary
        s <- arbitrary
        p <- arbitrary
        total_hash <- arbitrary
        blocks <- arbitrary
        total_len <- return $ sum . fmap HU.chunkSize $ blocks
        return $ case which of
            0 -> FS.File p t
            1 -> FS.ChecksumFile p t total_hash blocks
            2 -> FS.Symlink p t s
            3 -> FS.Directory p t
            4 -> FS.Error p p
