{-# LANGUAGE FlexibleContexts #-}

-- TODO remove HDBC.Sqlite3 from here and put it into the DB package
-- along with all queriesi (that will be prepared there)
-- The Reader will carry around the connection though
-- DB Monad? no needed, anyway we are going to use direct HDBC calls outside
-- and the driver specific ones in the DB module
-- How do we store the path of where the database is?

-- Symlink can be checked only on linu with the Unix package

module FSWatcher (
        iterateDirectory,
        Entry,
        Info(..),
        Error(..),
        filterErrors,
        filterInfo,
        findDiffs,
        testFS
        ) where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Crypto.Hash
import Data.Array
import Data.Conduit
import Data.Conduit.Lift
import Data.Int
import Data.Maybe (isJust)
import Data.Time.Clock
import System.Directory
import System.IO
import System.IO.Error
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
import qualified Database.HDBC as HS

import qualified DB
import Logger

logModule = "FSW"

-- Type returned from a Conduit looking for all files an directories
type Entry = Either Info Error

data Info = Info
    {
        entryPath :: String,
        isDirectory :: Bool,
        modificationTime :: UTCTime,
        isSymlink :: Bool,
        checksum :: Maybe (Digest MD5),
        blocks :: Maybe [Digest MD5]
    } deriving (Show)

data Error = Error {
        errorPath :: String,
        exception :: IOException
    } deriving (Show)

data IteratorConfiguration = IteratorConf {
        sqlSearch :: HS.Statement,
        followSymlink :: Bool
    }

dbHashBlockSize = 128*1024 :: Int64

-- | Iterate a path and given a Database connection will return all the modified
-- entries.
iterateDirectory :: (Monad m, MonadIO m, HS.IConnection c)
    => FilePath         -- ^ Directory path
    -> c                -- ^ Database connection
    -> Source m Entry   -- ^ Conduit source
iterateDirectory x c = do
    statement <- liftIO $ DB.sqlSelectModtime c
    runReaderC (IteratorConf statement False) (iterateDirectory' x)

-- TODO should traverse the directory and the database at the same time to
-- detect also deleted files.

-- Internal directory iterator
iterateDirectory' :: (Monad m, MonadIO m, MonadReader IteratorConfiguration m)
    => FilePath         -- ^ File path
    -> Source m Entry   -- ^ Sources of Entry
iterateDirectory' x = do
    send x True
    entries <- liftIO . tryIOError $ getDirectoryContents x
    case entries of
        Left e -> yield . Right $ Error x e
        Right l -> mapM_ recurse l
    where
        -- filter out special paths
        recurse "." = return ()
        recurse ".." = return ()
        recurse path = do
            let final_path = showString x. showString "/" $ path
            isDir <- liftIO $ doesDirectoryExist final_path
            isFile <- liftIO $ doesFileExist final_path
            test final_path isFile isDir

        test dest isFile isDir | isFile == isDir = return () -- doesn't exist
                               | isFile          = send dest False
                               | isDir           = iterateDirectory' dest

        send path isDir = do
            modTime <- liftIO . tryIOError $ getModificationTime path
            case modTime of
                Left e -> yield $ Right (Error x e)
                Right t -> do
                    ret <- checkDate path t
                    unless ret . yield . Left $ Info path isDir t False Nothing Nothing

-- | Returns true if the path is newer that the informations stored in the
-- database.
checkDate :: (MonadIO m, MonadReader IteratorConfiguration m)
    => FilePath     -- ^ Filepath
    -> UTCTime      -- ^ Modification date
    -> m Bool       -- ^ is newer?
checkDate path time = do
    s <- ask
    liftIO $ HS.execute (sqlSearch s) [HS.SqlString $! path, HS.SqlUTCTime $! time]
    m <- liftIO . HS.fetchRow .sqlSearch $ s
    return . isJust $ m

filterErrors :: (Monad m, MonadIO m) => Conduit Entry m Error
filterErrors = awaitForever $ either (\_ -> return()) yield

-- given entries as input filters out which we don't need to examine and all the
-- errors.
filterInfo :: (Monad m, MonadIO m) => Conduit Entry m Info
filterInfo = awaitForever $ either yield (\_ -> return())

md5 :: (Monad m, MonadIO m) => Conduit Entry m Entry
md5 = awaitForever hashit
    where
        hashit (Right x) = yield (Right x)
        hashit (Left x) | isDirectory x = yield . Left $ x
                        | otherwise     = liftIO (foo x) >>= yield

        foo :: Info -> IO Entry
        foo x = withFile (entryPath x) ReadMode (bar x) `catch`
            (return . Right . Error (entryPath x))

        bar x handle = do
            (h,b) <- md5hash dbHashBlockSize handle
            return . Left $ x { checksum = Just h, blocks = Just b }

{- Given a block size, an input ByteString and a Digest type return a serie of
 - block's checksums
 -}
hashblocks :: (HashAlgorithm d)
    => Int64                      -- ^ Block size
    -> Handle                     -- ^ Input file
    -> IO (Digest d, [Digest d])  -- ^ Source definition
hashblocks size h = (\(a,b) -> (hashFinalize a, b [])) <$> loop h (hashInit, id)
    where
        loop h (ctx, blocks) = do
            eof <- hIsEOF h
            if eof
            then return (ctx, blocks)
            else BS.hGet h (fromIntegral dbHashBlockSize) >>= \block ->
                    return (hashUpdate ctx block, blocks . (hash block :))

md5hash :: Int64 -> Handle -> IO (Digest MD5, [Digest MD5])
md5hash = hashblocks

-- Finds element that are different between the two sources
-- Usually one source is the local database, the other one is the current status
findDiffs :: (Eq o, Monad m) => Source m o -> Source m o -> Source m (Bool, o, o)
findDiffs s1 s2 =
    sequenceSources [s1, s2] $= awaitForever (\[a,b] -> yield (a==b, a, b))

testFS = do
    c <- DB.connect "test.db"
    DB.setup c
    b <- DB.verify c
    unless b $ error "db corrupted"
    -- should not use filterInfo, hashing can change an Info to an Error
    iterateDirectory "." c =$ {- filterInfo =$ -} md5 $$ CL.mapM_ (debugM logModule . show)
    DB.disconnect c

