{-# LANGUAGE TemplateHaskell #-}

module DB
    (DBConnection,
     connect,
     disconnect,
     verify,
     upgrade,
     getVersion,
     isFileNewer,
     upsertFile,
     insertFile,
     version)
    where

import qualified Database.HDBC as HS
import qualified Database.HDBC.Sqlite3 as HSD
import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Data.Maybe (isJust)
import Data.Array
import Data.Time.Clock
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL

import Data.Foldable (toList)

import HashUtils
import Logger
import FileEntry

type Connection = HSD.Connection

logModule = "DB"

-- array of prepare statement
-- used by all functions,no more exposure of the statements themselves
data DBConnection = DBC {
        handle :: HSD.Connection,
        statements :: Array Int (MVar HS.Statement)
    }

-- |Connect to the underlying SQL database
connect :: FilePath -> IO DBConnection
connect x = do
    c <- HSD.connectSqlite3 x
    HS.quickQuery c "PRAGMA journal_mode=WAL;" []
    HS.quickQuery c "PRAGMA encoding = \"UTF-8\";" []
    HS.runRaw c setupSQL
    HS.commit c
    prep_stmt <- mapM (\(a, s) -> do
        m <- HS.prepare c s
        s' <- newMVar m
        return (a, s')) db_statements
    return . DBC c $ array (0, length prep_stmt - 1) prep_stmt

db_statements = [
    -- get version
    (0, "SELECT value FROM schema_info WHERE key='version';"),
    -- check file in db based on modification time
    (1, "SELECT modificationTime FROM file WHERE path=? AND modificationTime<=?"),
    -- insert a file
    (2, "INSERT INTO file (path, modificationTime, directory, symlink, bhash, hash) VALUES (?, ?, ?, ?, ?, ?)"),
    -- all known paths
    (3, "SELECT path, hash FROM file ORDER BY path"),
    (4, "INSERT OR REPLACE INTO file (path, modificationTime, directory, symlink, bhash, hash) VALUES (?, ?, ?, ?, ?, ?)")]

disconnect :: DBConnection -> IO ()
disconnect (DBC c _) = HS.disconnect c

-- |Database schema version
version :: Int
version = 0

-- |Generates the SQL code to setup a database from scratch
setupSQL :: String
setupSQL = intercalate "\n" [
    "CREATE TABLE IF NOT EXISTS file (",
    " path TEXT NOT NULL PRIMARY KEY,",
    " modificationTime INTEGER NOT NULL,",
    " directory BOOLEAN NOT NULL,",
    " symlink TEXT NULL,",
    " bhash BLOB NULL,",
    " hash BLOB NULL",
    ");",
    "CREATE TABLE IF NOT EXISTS schema_info (",
    " key TEXT NOT NULL PRIMARY KEY,",
    " value TEXT NOT NULL",
    ");",
    "INSERT OR REPLACE INTO schema_info VALUES ('version', '"++show version++"');"]

-- | Verfies that the database structure looks ok
verify :: DBConnection -> IO Bool
verify (DBC c _) = do
    debugM logModule "Fetch tables"
    s <- HS.prepare c "SELECT name FROM sqlite_master WHERE type='table'"
    HS.execute s []
    tables <- (map HS.fromSql . concat <$> HS.fetchAllRows s) :: IO [String]
    debugM logModule $ "Found tables " ++ show tables
    let all_tables_there = elem "file" tables && elem "schema_info" tables
    debugM logModule $ "All tables: " ++ show all_tables_there
    return $! all_tables_there

-- Upgrades the schema
upgrade :: Int -> Int -> DBConnection -> IO ()
upgrade start end conn = return ()

-- |Returns the version of the database schema, if present.
-- Will raise an error in case of failure.
getVersion :: DBConnection -> IO Int
getVersion (DBC c _) = do
    s <- HS.prepare c "SELECT value FROM schema_info WHERE key='version';"
    HS.execute s []
    values <- HS.fetchAllRows s
    case values of
        [[x]] -> return $! HS.fromSql x
        _     -> ioError . userError $ "Version fetching failed"

sqlSelectAllKnownPaths :: HS.IConnection conn => conn -> IO HS.Statement
sqlSelectAllKnownPaths c = HS.prepare c "SELECT path, hash FROM file SORT BY path"

-- |Given a database, query index, and parameters it will perform the query
-- and apply the function to all the returned rows
withStatementAll :: (MonadIO m) =>
       DBConnection              -- ^ Database connection
    -> Int                       -- ^ Statement ID
    -> [HS.SqlValue]             -- ^ Values to pass as input
    -> ([[HS.SqlValue]] -> IO b) -- ^ All results sets lazily fetch are passed
                                 --   to this function
    -> m b                       -- ^ Returns the value in the source monad
withStatementAll (DBC c ss) idx params f = assert (elem idx (indices ss)) $ do
    liftIO . modifyMVar (ss ! idx) $ \stmt -> do
        HS.execute stmt params
        rows <- HS.fetchAllRows stmt
        r <- f rows
        return (stmt, r)

-- |Given a database, query index, and parameters it will perform the query
-- and apply the function to the first row only
withStatement :: (MonadIO m) =>
       DBConnection              -- ^ Database connection
    -> Int                       -- ^ Statement ID
    -> [HS.SqlValue]             -- ^ Values to pass as input
    -> ((Maybe [HS.SqlValue]) -> IO b) -- The single result 
    -> m b                       -- ^ Returns the value in the source monad
withStatement (DBC c ss) idx params f = assert (elem idx (indices ss)) $ do
    liftIO . modifyMVar (ss ! idx) $ \stmt -> do
        HS.execute stmt params
        row <- HS.fetchRow stmt
        r <- f row
        r' <- HS.fetchRow stmt
        assert (r' == Nothing) (HS.finish stmt)
        -- just in case there is more than one row
        return (stmt, r)

-- | Returns true if the path is newer that the informations stored in the
-- database.
isFileNewer :: (MonadIO m)
    => DBConnection
    -> FilePath     -- ^ Filepath
    -> UTCTime      -- ^ Modification date
    -> m Bool       -- ^ is newer?
isFileNewer db path time =
    withStatement db 1 [HS.SqlString path, HS.SqlUTCTime time] (\r -> do
            return . isJust $ r)

-- | Apply the Entry to any function passed to it to return a value
applyAll :: [Entry -> HS.SqlValue] -> Entry -> [HS.SqlValue]
applyAll xs e = zipWith ($) xs (repeat e)

-- |Inserts or updates an Entry object in the database
-- If it's an Error it's skipped, everything else is inserted in the database
upsertFile :: (MonadIO m) =>
       DBConnection -- ^ database connection
    -> Entry        -- ^ Entry to store in the database
    -> m ()
upsertFile db entry@Error{} = return ()
upsertFile db entry = do
    withStatement db 4
        (applyAll [HS.SqlString . entryPath,
                   HS.SqlUTCTime . modificationTime,
                   HS.SqlBool . isDirectory,
                   getSymlinkOrNull,
                   getBlocks,
                   HS.toSql . checksum] entry) (const $ return ())

-- |Insert a Entry object in the database
-- If it's an Error it's skipped, everything else is inserted in the database 
insertFile ::
       DBConnection -- ^ database connection
    -> Entry        -- ^ Entry to store in the database
    -> IO ()
insertFile db entry@Error{} = return ()
insertFile db entry = do
    withStatement db 2
        (applyAll [HS.SqlString . entryPath,
                   HS.SqlUTCTime . modificationTime,
                   HS.SqlBool . isDirectory,
                   getSymlinkOrNull,
                   getBlocks,
                   HS.toSql . checksum] entry) (const $ return ())

getSymlinkOrNull :: Entry -> HS.SqlValue
getSymlinkOrNull e@Symlink{} = HS.SqlString . target $ e
getSymlinkOrNull _ = HS.SqlNull

-- mallocArray
-- pokeArray
-- packCStringLen -> ByteString 

getBlocks :: Entry -> HS.SqlValue
getBlocks e@ChecksumFile{} = HS.SqlByteString . convert . blocks $ e
    where
        convert = BSL.toStrict
                   . BSB.toLazyByteString
                   . mconcat . toList
                   . fmap (\(CS a b) -> BSB.word32LE a `mappend` BSB.word32LE b)

-- | Execute a safe SQL Transaction
-- In case of error it will do a rollback, will execute a commit if there are no
-- errors
transaction ::
       DBConnection           -- ^ Connection
    -> (DBConnection -> IO a) -- ^ Code to Execute in the transaction
    -> IO a                   -- ^ Result
transaction db@(DBC c _) f =
    bracketOnError begin rollback $ \c -> do
        x <- f c
        liftIO commit
        return x
    where
        begin    = HS.quickQuery c "BEGIN TRANSACTION" [] >> return db
        rollback = const $ HS.quickQuery c "ROLLBACK" []
        commit   = HS.quickQuery c "COMMIT" []

