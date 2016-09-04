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
        md5blocks,
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.List as CL
import qualified Database.HDBC as HS

import qualified DB

-- Type returned from a Conduit looking for all files an directories
type Entry = Either Info Error

data Info = Info
    {
        entryPath :: String,
        isDirectory :: Bool,
        modificationTime :: UTCTime,
        isSymlink :: Bool,
        checksum :: Maybe (Digest MD5)
    } deriving (Show)

data Error = Error {
        errorPath :: String,
        exception :: IOException
    } deriving (Show)

data IteratorConfiguration = IteratorConf {
        sqlSearch :: HS.Statement,
        followSymlink :: Bool
    }

iterateDirectory :: (Monad m, MonadIO m, HS.IConnection c) => String -> c -> Source m Entry
iterateDirectory x c = do
    statement <- liftIO $ DB.sqlSelectModtime c
    runReaderC (IteratorConf statement False) (iterateDirectory' x)

-- Internal directory iterator
iterateDirectory' :: (Monad m, MonadIO m, MonadReader IteratorConfiguration m) => FilePath -> Source m Entry
iterateDirectory' x = do
    send x True
    entries <- liftIO . tryIOError $ getDirectoryContents x
    case entries of
        Left e -> yield . Right $ Error x e
        Right l -> mapM_ recurse l
    where
        folder = showString x

        -- filter out special paths
        recurse "." = return ()
        recurse ".." = return ()
        recurse path = do
            let final_path = folder . showString "/" $ path
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
                    unless ret . yield . Left $ Info path isDir t False Nothing

checkDate :: (MonadIO m, MonadReader IteratorConfiguration m) => String -> UTCTime -> m Bool
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

md5 :: (Monad m, MonadIO m) => Conduit Info m Info
md5 = awaitForever hashit
    where
        hashit x | isDirectory x = yield x
                 | otherwise     = do
                    h <- liftIO . fmap hashlazy . BL.readFile $ entryPath x
                    yield $ x { checksum = Just h }

{-
 - Given a block size, an input ByteString and a Digest type return a serie of
 - block's checksums
 -}
hashblocks :: (Monad m, HashAlgorithm d) => Int64 -> BL.ByteString -> Source m (Digest d)
hashblocks s = loop
    where
        loop ba = do
            let (b, b') = BL.splitAt s ba
            yield (hashlazy b) >> loop b'

md5blocks :: (Monad m) => Int64 -> BL.ByteString -> Source m (Digest MD5)
md5blocks = hashblocks

-- Finds element that are different between the two sources
-- Usually one source is the local database, the other one is the current status
findDiffs :: (Eq o, Monad m) => Source m o -> Source m o -> Source m (Bool, o, o)
findDiffs s1 s2 = sequenceSources [s1, s2] $= awaitForever inner
    where
        inner [a,b] = yield (a==b, a, b)

testFS = do
    c <- DB.connect "test.db"
    DB.setup c
    b <- DB.verify c
    if b
    then do
        iterateDirectory "." c =$ filterInfo =$ md5 $$ CL.mapM_ print
        DB.disconnect c
    else error "db corrupted"

