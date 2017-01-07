module FileEntry (
        Entry(..),
        FileDigest,
        isDirectory,
        isSymlink,
        addChecksum
    ) where

import Crypto.Hash
import Data.Time.Clock


type FileDigest = Digest MD5

-- Type returned from a Conduit looking for all files an directories
data Entry = File {
        entryPath :: String,
        modificationTime :: UTCTime
    }
           | ChecksumFile {
        entryPath :: String,
        modificationTime :: UTCTime,
        checksum :: FileDigest,
        blocks :: [FileDigest]
    }
           | Symlink {
        entryPath :: String,
        modificationTime :: UTCTime,
        target :: String
    }
           | Directory {
        entryPath :: String,
        modificationTime :: UTCTime
   }
           | Error {
        entryPath :: String,
        exception :: String
    }
    deriving (Show, Eq)

instance Ord Entry where
    a <= b = entryPath a <= entryPath b

isDirectory :: Entry -> Bool
isDirectory entry@Directory{} = True
isDirectory _ = False

isSymlink :: Entry -> Bool
isSymlink entry@Symlink{} = True
isSymlink _ = False

addChecksum :: Entry -> FileDigest -> [FileDigest] -> Entry
addChecksum (File a b) total blocks = ChecksumFile a b total blocks
addChecksum _ _ _  = error "Add Checksum to wrong entry type"

