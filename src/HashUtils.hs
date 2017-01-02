module HashUtils (
    digest2bytestring
    ) where

import qualified Data.ByteString as BS
import Crypto.Hash (digestFromByteString)
import qualified Data.ByteArray as BA
import Foreign.Ptr

digest2bytestring :: (HashAlgorithm a) => Digest a -> IO BS.ByteString
digest2bytestring = flip withByteArray . peekArray (length digest)

bytestring2digest :: (HashAlgorithm a) => BS.ByteString -> Maybe (Digest a)
bytestring2digest = digestFromByteString

