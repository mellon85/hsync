module MyArbitrary where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Word (Word8)
import qualified Database.HDBC as HS
import Data.Time.Clock
import qualified Data.ByteString as BS
import Data.Time.Calendar

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

