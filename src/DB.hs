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
import Control.Monad.Catch
import Data.Maybe (isJust)
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

data DBConnection = DBC {
        handle :: HSD.Connection
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

disconnect :: DBConnection -> IO ()
disconnect (DBC c) = HS.disconnect c

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
verify (DBC c) = do
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
getVersion (DBC c) = do
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
withStatementAll :: (MonadMask m, MonadCatch m, MonadIO m) =>
       DBConnection              -- ^ Database connection
    -> String                    -- ^ Statement ID
    -> [HS.SqlValue]             -- ^ Values to pass as input
    -> ([[HS.SqlValue]] -> m b) -- ^ All results sets lazily fetch are passed
                                 --   to this function
    -> m b                       -- ^ Returns the value in the source monad
withStatementAll (DBC c) stmt params f =
    bracket (liftIO $ HS.prepare c stmt) (liftIO . HS.finish) $ \pstmt -> do
        rows <- liftIO $ do
            HS.execute pstmt params
            HS.fetchAllRows pstmt
        f rows

-- |Given a database, query index, and parameters it will perform the query
-- and apply the function to the first row only
withStatement :: (MonadMask m, MonadIO m) =>
       DBConnection                   -- ^ Database connection
    -> String                         -- ^ Statement ID
    -> [HS.SqlValue]                  -- ^ Values to pass as input
    -> ((Maybe [HS.SqlValue]) -> m b) -- The single result 
    -> m b                            -- ^ Returns the value in the source monad
withStatement (DBC c) stmt params f = do
    row <- bracket (liftIO $ HS.prepare c stmt) (liftIO . HS.finish) $ \pstmt ->
        liftIO $ do
            HS.execute pstmt params
            HS.fetchRow pstmt
    f row

-- | Returns true if the path is newer that the informations stored in the
-- database.
isFileNewer :: (MonadMask m, MonadIO m)
    => DBConnection
    -> FilePath     -- ^ Filepath
    -> UTCTime      -- ^ Modification date
    -> m Bool       -- ^ is newer?
isFileNewer db path time =
    withStatement db
        "SELECT modificationTime FROM file WHERE path=? AND modificationTime<=?"
        [HS.SqlString path, HS.SqlUTCTime time] (\r -> do return . isJust $ r)

-- | Apply the Entry to any function passed to it to return a value
applyAll :: [Entry -> HS.SqlValue] -> Entry -> [HS.SqlValue]
applyAll xs e = zipWith ($) xs (repeat e)

-- |Inserts or updates an Entry object in the database
-- If it's an Error it's skipped, everything else is inserted in the database
upsertFile :: (MonadMask m, MonadIO m) =>
       DBConnection -- ^ database connection
    -> Entry        -- ^ Entry to store in the database
    -> m ()
upsertFile db entry@Error{} = return ()
upsertFile db entry = do
    liftIO $ infoM "DB" $ "upsert: " ++ (show entry)
    withStatement db
        "INSERT OR REPLACE INTO file (path, modificationTime, directory, symlink, bhash, hash) VALUES (?, ?, ?, ?, ?, ?)"
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
    liftIO $ infoM "DB" $ "insert: " ++ (show entry)
    withStatement db
        "INSERT INTO file (path, modificationTime, directory, symlink, bhash, hash) VALUES (?, ?, ?, ?, ?, ?)"
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
transaction :: (MonadMask m, MonadIO m) =>
       DBConnection          -- ^ Connection
    -> (DBConnection -> m a) -- ^ Code to Execute in the transaction
    -> m a                   -- ^ Result
transaction db@(DBC c) f =
    bracketOnError begin rollback $ \c -> do
        x <- f c
        commit
        return x
    where
        begin      = liftIO $ HS.quickQuery c "BEGIN TRANSACTION" [] >> return db
        rollback _ = liftIO $ HS.quickQuery c "ROLLBACK" []
        commit     = liftIO $ HS.quickQuery c "COMMIT" []
