{-# LANGUAGE TemplateHaskell #-}

module DB
    (connect,
     disconnect,
     verify,
     upgrade,
     getVersion,
     DBConnection,
     isFileNewer,
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
    HS.quickQuery c "PRAGMA encoding = \"UTF-8\";" []
    --HS.quickQuery c "PRAGMA journal_mode=WAL;" []
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
    (2, "INSERT INTO file (path, modificationTime, bhash, hash) VALUES (?, ?, ?, ?)"),
    -- all known paths
    (3, "SELECT path, hash FROM file ORDER BY path")]

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
    " bhash BLOB,",
    " hash BLOB,",
    " symlink TEXT",
    ");",
    "CREATE TABLE IF NOT EXISTS schema_info (",
    " key TEXT NOT NULL PRIMARY KEY,",
    " value TEXT NOT NULL",
    ");",
    "INSERT OR REPLACE INTO schema_info VALUES ('version', '"++show version++"');"]

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

sqlInsertFile :: HS.IConnection conn => conn -> IO HS.Statement
sqlInsertFile c = HS.prepare c
    "INSERT INTO file (path, modificationTime, bhash, hash) VALUES (?, ?, ?, ?)"

sqlSelectAllKnownPaths :: HS.IConnection conn => conn -> IO HS.Statement
sqlSelectAllKnownPaths c = HS.prepare c "SELECT path, hash FROM file SORT BY path"

-- |Given a database, query index, and parameters it will perform the query
-- and apply the function to all the returned rows
withStatementAll :: (MonadIO m) => DBConnection -> Int -> [HS.SqlValue] -> ([[HS.SqlValue]] -> IO b) -> m b
withStatementAll (DBC c ss) idx params f = assert (elem idx (indices ss)) $ do
    liftIO . modifyMVar (ss ! idx) $ \stmt -> do
        HS.execute stmt params
        rows <- HS.fetchAllRows stmt
        r <- f rows
        return (stmt, r)

-- |Given a database, query index, and parameters it will perform the query
-- and apply the function to the first row only
withStatement :: (MonadIO m) => DBConnection -> Int -> [HS.SqlValue] -> ((Maybe [HS.SqlValue]) -> IO b) -> m b
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

applyAll e [] = []
applyAll e (x:xs) = (x e) : applyAll e xs

insertFile :: (HS.IConnection conn)
    => conn
    -> Entry
    -> IO ()
insertFile c entry@ChecksumFile{} = do
    withStatement db 2
        (applyAll [HS.SqlString . entryPath,
                   HS.SqlUTCTime . modificationTime,
                   HS.SqlByteString . blocks,
                   HS.SqlByteString . checksum] entry) (return ())

insertFile c entry@Directory{} = do
    withStatement db 2
        (applyAll [HS.SqlString . entryPath,
                   HS.SqlUTCTime . modificationTime,
                   HS.SqlByteString . blocks,
                   HS.SqlByteString . checksum] entry) (return ())

insertFile c entry@Error{} = return ()

-- Directory and File
insertFile c entry = do
    withStatement db 2
        ((applyAll [HS.SqlString . entryPath,
                   HS.SqlUTCTime . modificationTime] entry) ++
            [HS.SqlNull, HS.SqlNull]) (return ())


-- | Execute a safe SQL Transaction
-- In case of error it will do a rollback, will execute a commit if there are no
-- errors
sqlTransaction :: HS.IConnection conn
    => conn             -- ^ Connection
    -> (conn -> IO a)   -- ^ Transaction function
    -> IO a             -- ^ Result
sqlTransaction c f =
    bracketOnError
        (HS.quickQuery c "BEGIN TRANSACTION" [])
        (\_ -> HS.quickQuery c "ROLLBACK" [])
        (\_ -> f c >>= (\x -> HS.quickQuery c "COMMIT" [] >> return x))

