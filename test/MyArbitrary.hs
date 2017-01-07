module MyArbitrary where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Word (Word8)
import qualified Database.HDBC as HS
import Data.Time.Clock
import qualified Data.ByteString as BS
import Data.Time.Calendar
import Crypto.Hash
import qualified FSWatcher as FS
import Control.Exception.Base
import FileEntry

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

instance Arbitrary Entry where
    arbitrary = do
        which <- choose (0,4) :: Gen Int
        t <- arbitrary
        s <- arbitrary
        p <- arbitrary
        ss <- arbitrary
        hs <- return $ BS.pack ss
        return $ case which of
            0 -> File p t
            1 -> ChecksumFile p t (hash hs) [hash hs]
            2 -> Directory p t
            3 -> Symlink p t s
            4 -> Error p p
