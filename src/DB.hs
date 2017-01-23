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
     allPaths,
     getEntry,
     transaction,
     version)
    where

import qualified Database.HDBC as HS
import qualified Database.HDBC.Sqlite3 as HSD
import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Maybe (isJust)
import Data.Time.Clock
import Data.Conduit hiding (connect)
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteArray as BA
import Crypto.Hash (digestFromByteString)
import Database.HDBC (fromSql)
import Control.Monad.Catch

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
    tables <- HS.getTables (dbhandle c)
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


-- |Given a database, query index, and parameters it will perform the query
-- and apply the function to all the returned rows
withStatementAll :: (MonadIO m, MonadMask m) =>
       DBConnection              -- ^ Database connection
    -> String                    -- ^ Statement
    -> [HS.SqlValue]             -- ^ Values to pass as input
    -> ([[HS.SqlValue]] -> m b)  -- ^ All results sets lazily fetch are passed
                                 --   to this function
    -> m b                       -- ^ Returns the value in the source monad
withStatementAll c sql params f = do
    bracket init last core
    where
        init = do
            stmt <- liftIO $ HS.prepare (dbhandle c) sql
            liftIO $ HS.execute stmt params
            return $ (dbhandle c, stmt)

        last (c, _) = liftIO $ HS.rollback c

        core (c, stmt) = do
            rows <- liftIO $ HS.fetchAllRows stmt
            r <- f rows
            liftIO $ HS.commit c
            return r

-- | Given a database, query index, and parameters it will perform the query
-- and apply the function to the first row only
withStatement :: (MonadIO m) =>
       DBConnection              -- ^ Database connection
    -> String                    -- ^ Statement
    -> [HS.SqlValue]             -- ^ Values to pass as input
    -> (([[HS.SqlValue]]) -> IO b) -- The single result
    -> m b                       -- ^ Returns the value in the source monad
withStatement c sql params f = do
    liftIO $ HS.withTransaction (dbhandle c) $ \db -> do
        row <- HS.quickQuery' (dbhandle c) sql params
        f row

withStatementMany :: (MonadIO m) =>
       DBConnection              -- ^ Database connection
    -> String                    -- ^ Statement
    -> [[HS.SqlValue]]           -- ^ Values to pass as input
    -> m ()                      -- ^ Returns the value in the source monad
withStatementMany c sql params =
    liftIO $ HS.withTransaction (dbhandle c) $ \db -> do
        stmt <- HS.prepare db sql
        HS.executeMany stmt params

-- | Returns true if the path is newer that the informations stored in the
-- database.
isFileNewer :: (MonadIO m)
    => DBConnection
    -> FilePath     -- ^ Filepath
    -> UTCTime      -- ^ Modification date
    -> m Bool       -- ^ is newer?
isFileNewer db path time =
    withStatement db
        "SELECT modificationTime FROM file WHERE path=? AND modificationTime<=?"
        [HS.SqlString path, HS.SqlUTCTime time] (return . (>0) . length)


-- |Inserts or updates an Entry object in the database
-- If it's an Error it's skipped, everything else is inserted in the database
upsertFile :: (MonadIO m) =>
       DBConnection -- ^ database connection
    -> [Entry]      -- ^ Entry to store in the database
    -> m ()
upsertFile db entries = withStatementMany db
        "INSERT OR REPLACE INTO file (path, modificationTime, directory, symlink, bhash, hash) VALUES (?, ?, ?, ?, ?, ?)"
    . map entryToSql . filter (not . isError) $ entries

-- |Insert a Entry object in the database
-- If it's an Error it's skipped, everything else is inserted in the database
insertFile :: (MonadIO m) =>
       DBConnection -- ^ database connection
    -> [Entry]      -- ^ Entry to store in the database
    -> m ()
insertFile db entries = withStatementMany db
    "INSERT INTO file (path, modificationTime, directory, symlink, bhash, hash) VALUES (?, ?, ?, ?, ?, ?)"
    . map entryToSql . filter (not . isError) $ entries

allPaths :: (MonadIO m, MonadResource m) =>
        DBConnection
     -> Source m (String)
allPaths db = bracketP init clean loop
    where
        init = do stmt <- HS.prepare (dbhandle db)
                      "SELECT path FROM file ORDER BY path"
                  HS.execute stmt []
                  return stmt

        clean = HS.finish

        loop stmt = do
            row <- liftIO $ HS.fetchRow stmt
            case row of
                Nothing     -> return ()
                Just [path] -> do yield $ (HS.fromSql path :: String)
                                  loop stmt

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
       DBConnection     -- ^ Connection
    -> (DBConnection -> IO a)   -- ^ Transaction function
    -> IO a             -- ^ Result
transaction db f = HS.withTransaction (dbhandle db) (\_ -> f db)

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

entryToSql :: Entry -> [HS.SqlValue]
entryToSql = applyAll [
    HS.SqlString . entryPath,
    HS.SqlUTCTime . modificationTime,
    HS.SqlBool . isDirectory,
    getSymlinkOrNull,
    getFileBlobField blocks,
    getFileBlobField ((\x->[x]) . checksum)]
    where
        -- | Apply the Entry to any function passed to it to return a value
        applyAll :: [Entry -> HS.SqlValue] -> Entry -> [HS.SqlValue]
        applyAll xs e = map (\f -> f e) xs

getEntry :: (MonadIO m) =>
        DBConnection    -- | Database connection
     -> String          -- | File path to search
     -> m (Maybe Entry) -- | The Entry
getEntry db fpath = do
    row <- liftIO $ HS.withTransaction (dbhandle db) $ \db -> do
        HS.quickQuery' db sql [HS.toSql fpath]
    return . sqlToEntry $ head row
    where
        sql = "SELECT modificationTime, directory, symlink, bhash, hash FROM file WHERE path=?"

        sqlToEntry :: [HS.SqlValue] -> Maybe Entry
        sqlToEntry [] = Nothing
        sqlToEntry [timestamp, dir, sym, bhash, hash] = let
            time = fromSql timestamp
            directory = fromSql dir :: Bool
            symlink = fromSql sym
            bhash' = deserializeHashes  $ fromSql bhash -- might be null
            hash' = head . deserializeHashes  $ fromSql bhash -- might be null
            in Just $! case (directory, symlink, hash) of
                (_,HS.SqlByteString _,_) -> Symlink { entryPath = fpath,
                                        modificationTime = time,
                                        target = fromSql sym}
                (True,_,_) -> Directory { entryPath = fpath,
                                          modificationTime = time }
                (_,_,HS.SqlNull) -> File { entryPath = fpath,
                                           modificationTime = time }
                (_,_,_) -> ChecksumFile { entryPath = fpath,
                                          modificationTime = time,
                                          blocks = bhash',
                                          checksum = hash' }

