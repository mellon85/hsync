module HashUtils (
    digest2bytestring
    ) where

import qualified Data.ByteString as BS
import Crypto.Hash
import qualified Data.ByteArray as BA
import Foreign.Ptr
import Foreign.Marshal.Array (peekArray)

digest2bytestring :: (HashAlgorithm a, BA.ByteArrayAccess a) => Digest a -> IO BS.ByteString
digest2bytestring digest = BS.pack <$> BA.withByteArray digest (peekArray (BA.length digest))

bytestring2digest :: (HashAlgorithm a) => BS.ByteString -> Maybe (Digest a)
bytestring2digest = digestFromByteString
