module FileEntry where

import Crypto.Hash
import Data.Time.Clock

type FileDigest = Digest MD5

-- Type returned from a Conduit looking for all files an directories
data Entry = File {
        entryPath :: String,
        modificationTime :: UTCTime,
        isSymlink :: Bool
    }
           | ChecksumFile {
        entryPath :: String,
        modificationTime :: UTCTime,
        isSymlink :: Bool,
        checksum :: FileDigest,
        blocks :: [FileDigest]
    }
           | Directory {
        entryPath :: String,
        modificationTime :: UTCTime,
        isSymlink :: Bool
   }
           | Error {
        entryPath :: String,
        exception :: String
    }
    deriving (Show, Eq)

instance Ord Entry where
    a <= b = entryPath a <= entryPath b
    
addChecksum :: Entry -> FileDigest -> [FileDigest] -> Entry
addChecksum (File a b c) total blocks = ChecksumFile a b c total blocks
addChecksum _ _ _ = error "Add Checksum to wrong entry type"

