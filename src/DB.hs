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
     transaction,
     version)
    where

import qualified Database.HDBC as HS
import qualified Database.HDBC.Sqlite3 as HSD
import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Data.Maybe (isJust)
import Data.Time.Clock
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteArray as BA
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Crypto.Hash (digestFromByteString)

import Logger
import FileEntry

type Connection = HSD.Connection

logModule = "DB"

-- array of prepare statement
-- used by all functions,no more exposure of the statements themselves
data DBConnection = DBC {
        dbhandle :: HSD.Connection
    }

-- |Connect to the underlying SQL database
connect :: FilePath -> IO DBConnection
connect x = do
    c <- HSD.connectSqlite3 x
    HS.quickQuery c "PRAGMA encoding = \"UTF-8\";" []
    --HS.quickQuery c "PRAGMA journal_mode=WAL;" []
    HS.runRaw c setupSQL
    HS.commit c
    return $ DBC c

db_statements = M.fromList [
    -- get version
    (0, "SELECT value FROM schema_info WHERE key='version';"),
    -- check file in db based on modification time
    (1, "SELECT modificationTime FROM file WHERE path=? AND modificationTime<=?"),
    -- insert a file
    (2, "INSERT INTO file (path, modificationTime, directory, symlink, bhash, hash) VALUES (?, ?, ?, ?, ?, ?)"),
    -- all known paths
    (3, "SELECT path, hash FROM file ORDER BY path"),
    (4, "INSERT OR REPLACE INTO file (path, modificationTime, directory, symlink, bhash, hash) VALUES (?, ?, ?, ?, ?, ?)"),
    (5, "SELECT path, hash FROM file SORT BY path")]

disconnect :: DBConnection -> IO ()
disconnect = HS.disconnect . dbhandle

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
verify c = do
    debugM logModule "Fetch tables"
    s <- HS.prepare (dbhandle c) "SELECT name FROM sqlite_master WHERE type='table'"
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
getVersion c = do
    s <- HS.prepare (dbhandle c) "SELECT value FROM schema_info WHERE key='version';"
    HS.execute s []
    values <- HS.fetchAllRows s
    case values of
        [[x]] -> return $! HS.fromSql x
        _     -> ioError . userError $ "Version fetching failed"

allPaths :: DBConnection -> IO HS.Statement
allPaths c = HS.prepare (dbhandle c) "SELECT path, hash FROM file SORT BY path"

-- |Given a database, query index, and parameters it will perform the query
-- and apply the function to all the returned rows
withStatementAll :: (MonadIO m) =>
       DBConnection              -- ^ Database connection
    -> Int                       -- ^ Statement ID
    -> [HS.SqlValue]             -- ^ Values to pass as input
    -> ([[HS.SqlValue]] -> m b) -- ^ All results sets lazily fetch are passed
                                 --   to this function
    -> m b                       -- ^ Returns the value in the source monad
withStatementAll c idx params f = do
    stmt <- liftIO . HS.prepare (dbhandle c) $ db_statements ! idx
    liftIO $ HS.execute stmt params
    rows <- liftIO $ HS.fetchAllRows stmt
    r <- f rows
    return r

-- |Given a database, query index, and parameters it will perform the query
-- and apply the function to the first row only
withStatement :: (MonadIO m) =>
       DBConnection              -- ^ Database connection
    -> Int                       -- ^ Statement ID
    -> [HS.SqlValue]             -- ^ Values to pass as input
    -> ((Maybe [HS.SqlValue]) -> m b) -- The single result
    -> m b                       -- ^ Returns the value in the source monad
withStatement c idx params f = do
    stmt <- liftIO $ HS.prepare (dbhandle c) $ db_statements ! idx
    liftIO $ HS.execute stmt params
    row <- liftIO $ HS.fetchRow stmt
    r <- f row
    r' <- liftIO $ HS.fetchRow stmt
    assert (r' == Nothing) (liftIO $ HS.finish stmt)
    -- just in case there is more than one row
    return r

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
applyAll xs e = map (\f -> f e) xs

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
                   getFileBlobField blocks,
                   getFileBlobField ((\x->[x]) . checksum)] entry)
        (return . pure ())

-- |Insert a Entry object in the database
-- If it's an Error it's skipped, everything else is inserted in the database
insertFile :: (MonadIO m) =>
       DBConnection -- ^ database connection
    -> Entry        -- ^ Entry to store in the database
    -> m ()
insertFile db entry@Error{} = return ()
insertFile db entry = do
    withStatement db 2
        (applyAll [HS.SqlString . entryPath,
                   HS.SqlUTCTime . modificationTime,
                   HS.SqlBool . isDirectory,
                   getSymlinkOrNull,
                   getFileBlobField blocks,
                   getFileBlobField ((\x->[x]) . checksum)] entry)
        (return . pure ())

getSymlinkOrNull :: Entry -> HS.SqlValue
getSymlinkOrNull e@Symlink{} = HS.SqlString . target $ e
getSymlinkOrNull _ = HS.SqlNull

getFileBlobField :: (Entry -> [FileDigest]) -> Entry -> HS.SqlValue
getFileBlobField f e@ChecksumFile{} = HS.SqlByteString . serializeHashes . f $ e
getFileBlobField _ _ = HS.SqlNull

-- | Execute a safe SQL Transaction
-- In case of error it will do a rollback, will execute a commit if there are no
-- errors
transaction ::
       DBConnection    -- ^ Connection
    -> (DBConnection -> IO a)   -- ^ Transaction function
    -> IO a             -- ^ Result
transaction db f = let c = dbhandle db in
    bracketOnError
        (HS.quickQuery c "BEGIN TRANSACTION" [])
        (\_ -> HS.quickQuery c "ROLLBACK" [])
        (\_ -> do
            x <- f db
            HS.quickQuery c "COMMIT" []
            return x)

serializeHashes :: [FileDigest] -> BS.ByteString
serializeHashes = BL.toStrict . BL.fromChunks . map BA.convert

deserializeHashes :: BS.ByteString -> [FileDigest]
deserializeHashes hashes = go $ BS.splitAt block hashes
    where
        block = 64
        go (h, r) | BS.length h == 0 = []
                  | otherwise        = case digestFromByteString h of
                        Just d -> d : go (BS.splitAt block r)
                        _      -> []

