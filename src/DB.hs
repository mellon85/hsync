{-# LANGUAGE TemplateHaskell #-}

module DB
    (connect,
     disconnect,
     setup,
     verify,
     upgrade,
     sqlSelectModtime,
     sqlInsertFile,
     getVersion,
     version)
    where

import qualified Database.HDBC as HS
import qualified Database.HDBC.Sqlite3 as HSD
import Data.List
import Control.Monad

type Connection = HSD.Connection

-- |Connect to the underlying SQL database
connect :: FilePath -> IO HSD.Connection
connect x = do
    c <- HSD.connectSqlite3 x
    HS.quickQuery c "PRAGMA journal_mode=WAL" []
    return c

disconnect :: (HS.IConnection a) => a -> IO ()
disconnect = HS.disconnect

-- |Database schema version
version :: Int
version = 0

-- |Generates the SQL code to setup a database from scratch
setupSQL :: String
setupSQL = intercalate "\n" [
    "CREATE TABLE IF NOT EXISTS file (",
    " path TEXT NOT NULL PRIMARY KEY,",
    " modificationTime INTEGER NOT NULL,",
    " blob BLOB,",
    " md5 BLOB",
    ");",
    "CREATE TABLE IF NOT EXISTS schema_info (",
    " key TEXT NOT NULL PRIMARY KEY,",
    " value TEXT NOT NULL",
    ");",
    "INSERT OR REPLACE INTO schema_info VALUES ('version', '"++show version++"');"]

verify :: (HS.IConnection a) => a -> IO Bool
verify c = do
    s <- HS.prepare c "SELECT name FROM sqlite_master WHERE type='table'"
    HS.execute s []
    tables <- (map HS.fromSql . concat <$> HS.fetchAllRows s) :: IO [String]
    let all_tables_there = elem "file" tables && elem "schema_info" tables
    return $! all_tables_there

-- Upgrades the schema
upgrade :: (HS.IConnection a) => Int -> Int -> a -> IO String
upgrade start end conn = return ""

-- |Returns the version of the database schema, if present.
-- Will raise an error in case of failure.
getVersion :: (HS.IConnection a) => a -> IO Int
getVersion c = do
    s <- HS.prepare c "SELECT value FROM schema_info WHERE key='version';"
    HS.execute s []
    values <- HS.fetchAllRows s
    case values of
        [[x]] -> return $! HS.fromSql x
        _     -> ioError . userError $ "Version fetching failed"

-- |Setup a database given a connection
setup :: (HS.IConnection a) => a -> IO ()
setup c = do
    HS.runRaw c setupSQL
    HS.commit c
    return ()

sqlSelectModtime :: HS.IConnection conn => conn -> IO HS.Statement
sqlSelectModtime c = HS.prepare c
    "SELECT modificationTime FROM file WHERE path=? AND modificationTime<=?"

sqlInsertFile :: HS.IConnection conn => conn -> IO HS.Statement
sqlInsertFile c = HS.prepare c
    "INSERT INTO file (path, modificationTime, blob, md5) VALUES (?, ?, ?, ?)"
