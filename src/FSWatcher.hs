{-# LANGUAGE FlexibleContexts #-}

module FSWatcher (
        iterateDirectory,
        Entry(..),
        filterErrors,
        findDiffs,
        testFS,
        module FileEntry
        ) where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Crypto.Hash
import Data.Conduit
import Data.Conduit.Lift
import Data.Int
import Data.Time.Clock
import System.Directory
import System.IO
import System.IO.Error
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
import qualified Database.HDBC as HS

import qualified DB
import Logger
import FileEntry
import HashUtils

import Data.Sequence as S

logModule = "FSW"

data Comparison a = NewLeft a
                  | NewRight a
                  | Collision a a
    deriving (Show)

data IteratorConfiguration = IteratorConf {
        connection :: DB.DBConnection,
        followSymlink :: Bool -- currently always false
    }

-- | Iterate a path and given a Database connection will return all the modified
-- entries.
iterateDirectory :: (Monad m, MonadIO m)
    => FilePath         -- ^ Directory path
    -> DB.DBConnection     -- ^ Database connection
    -> Source m Entry   -- ^ Conduit source
iterateDirectory x c = do
    runReaderC (IteratorConf c False) (iterateDirectory' x)

-- Internal directory iterator
iterateDirectory' :: (Monad m, MonadIO m, MonadReader IteratorConfiguration m)
    => FilePath         -- ^ File path
    -> Source m Entry   -- ^ Sources of Entry
iterateDirectory' x = do
    send x True
    entries <- liftIO . tryIOError $ getDirectoryContents x
    case entries of
        Left e -> yield $ Error x $ displayException e
        Right l -> mapM_ recurse l
    where
        -- filter out special paths
        recurse "." = return ()
        recurse ".." = return ()
        recurse path = do
            let final_path = showString x . showString "/" $ path
            isDir <- liftIO $ doesDirectoryExist final_path
            isFile <- liftIO $ doesFileExist final_path
            if isFile == isDir then return () -- doesn't exist
            else if isFile then send final_path False
            else iterateDirectory' final_path

        -- Send will read the modification date. If not possible will mark
        -- the Entry as error
        send path isDir = do
            modTime <- liftIO . tryIOError $ getModificationTime path
            c <- ask  >>= return . connection
            case modTime of
                Left e -> yield $ Error path $ displayException e
                Right t -> do
                    ret <- liftIO $ DB.isFileNewer c path t
                    sym <- liftIO $ pathIsSymbolicLink path
                    unless ret . yield $ make path t
            where
                make | isDir     = Directory
                     | otherwise = File

filterErrors :: (Monad m, MonadIO m) => Conduit Entry m Entry
filterErrors = awaitForever match
    where
        match x@Error{} = yield x
        match _ = return ()

hashEntry :: (Monad m, MonadIO m) => Conduit Entry m Entry
hashEntry = awaitForever hashit
    where
        hashit f@File{} = liftIO (foo f) >>= yield
        hashit x = yield x

        foo :: Entry -> IO Entry
        foo x = withFile (entryPath x) ReadMode (hash x) `catch`
                        (return . Error (entryPath x) . io2s)

        io2s :: IOException -> String
        io2s = displayException

        hash x handle = do
            (total, hashes) <- hashblocks handle
            return $ addChecksum x total hashes -- not correct

hashblocks :: Handle                         -- ^ Input file
           -> IO (AdlerHash, Seq ChunkedSum) -- ^ File and chunk hashes
hashblocks handle = runConduit $ chunksumC handle .| sinkSeq

-- Finds element that are different between the two sources
-- Usually one source is the local database, the other one is the current status
findDiffs :: (Monad m) => ConduitM () Entry m () -> ConduitM () Entry m () -> ConduitM () (Comparison Entry) m ()
findDiffs s1 s2 = do
    -- get first elements from both
    v1 <- lift $ newResumableSource s1 $$++ await
    v2 <- lift $ newResumableSource s2 $$++ await
    recurse v1 v2
    where
        get src = lift $ src $$++ await

        -- do all the cases and tail recursively yield the differences
        recurse (_, Nothing) (_, Nothing) = return ()
        recurse v1@(_, Nothing) (rs2, Just b) = do
            yield (NewRight b)
            v2 <- get rs2
            recurse v1 v2

        recurse (rs1, Just a) v2@(_, Nothing) = do
            yield (NewLeft a)
            v1 <- get rs1
            recurse v1 v2

        recurse v1@(rs1, Just a) v2@(rs2, Just b)
            | entryPath a == entryPath b = do
                when (a /= b) (yield $ Collision a b)
                v1 <- get rs1
                v2 <- get rs2
                recurse v1 v2
            | entryPath a < entryPath b = do
                yield $ NewLeft a
                v1 <- get rs1
                recurse v1 v2
            | entryPath a > entryPath b = do
                yield $ NewRight b
                v2 <- get rs2
                recurse v1 v2


testFS = do
    c <- DB.connect "test.db"
    b <- DB.verify c
    unless b $ error "db corrupted"
    -- should not use filterInfo, hashing can change an Info to an Error
    iterateDirectory "." c =$ hashEntry $$ CL.mapM_ (debugM logModule . show)
    DB.disconnect c

