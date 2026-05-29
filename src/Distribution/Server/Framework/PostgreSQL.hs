{-# LANGUAGE OverloadedStrings #-}
-- | PostgreSQL persistence layer for hackage-server.
--
-- Architecture (same as acid-state):
--
-- * State lives in an 'MVar' for fast in-memory reads
-- * Each update INSERTs an event row to PostgreSQL (durable)
-- * Periodically, 'createCheckpoint' writes the full state to
--   checkpoint tables and truncates event rows
-- * On startup, load checkpoint + replay events since
--
-- All database access goes through a connection 'Pool' from
-- @resource-pool@, ensuring thread safety under concurrent requests.
--
-- Transactional code uses the 'PgTx' monad (from
-- "Distribution.Server.Framework.PgTx") to enforce that all operations
-- within a transaction use the same connection.
module Distribution.Server.Framework.PostgreSQL
  ( -- * State handle
    AcidState(..)
    -- * Construction
  , mkAcidState
    -- * Queries and updates
  , queryPg
  , updatePg
    -- * Checkpoints and closing
  , createCheckpoint
  , closeAcidState
    -- * Connection pool
  , PgConnection
  , connectPg
  , disconnectPg
    -- * Non-transactional beam queries (e.g. loading state)
  , runBeamPg
    -- * Running transactions
  , runPgTx
    -- * Schema loading
  , executeSchema
  ) where

import Control.Monad (void)
import Control.Concurrent.MVar
import Control.Monad.Reader (runReader, runReaderT)
import qualified Control.Monad.State.Lazy as State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Pool (Pool, newPool, withResource, defaultPoolConfig)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import Database.PostgreSQL.Simple.Transaction (withTransactionModeRetry,
    TransactionMode(..), IsolationLevel(..), ReadWriteMode(..),
    isSerializationError)

import Database.Beam.Postgres (Pg, runBeamPostgres)

import Distribution.Server.Framework.EventSourcing (Query, Update)
import Distribution.Server.Framework.PgTx (PgTx(..), pgTxRaw)


-- * Connection pool

-- | A thread-safe PostgreSQL connection pool.
type PgConnection = Pool PG.Connection

-- | Create a connection pool to PostgreSQL.
connectPg :: ByteString -> IO PgConnection
connectPg connStr = newPool $ defaultPoolConfig
  (PG.connectPostgreSQL connStr)  -- create a connection
  PG.close                        -- destroy a connection
  60                              -- max idle time (seconds)
  10                              -- max connections

-- | Destroy the connection pool.
disconnectPg :: PgConnection -> IO ()
disconnectPg _ = return ()  -- pool handles cleanup


-- * Non-transactional beam queries

-- | Run a beam query using a connection from the pool.
-- For loading state on startup (no transaction needed).
-- Do NOT use inside 'PgTx' — use 'beamTx' instead.
runBeamPg :: PgConnection -> Pg a -> IO a
runBeamPg pool action = withResource pool $ \conn -> runBeamPostgres conn action


-- * Running transactions

-- | Run a 'PgTx' action: grab a connection from the pool,
-- wrap in a SERIALIZABLE transaction, and automatically retry
-- on serialization failures.
runPgTx :: PgConnection -> PgTx a -> IO a
runPgTx pool (PgTx action) = withResource pool $ \conn ->
  withTransactionModeRetry
    (TransactionMode Serializable ReadWrite)
    isSerializationError
    conn
    (runReaderT action conn)


-- * Schema loading

-- | Execute a schema file as a single multi-statement query
-- inside a transaction.
executeSchema :: PgConnection -> String -> IO ()
executeSchema pool schemaFile = runPgTx pool $ pgTxRaw $ \conn ->
  void $ PG.execute_ conn (PG.Query (BS8.pack schemaFile))


-- * State handle

-- | The state handle, replacing acid-state's @AcidState@.
--
-- Each state component (packages, users, tags, etc.) has its own
-- 'AcidState' with:
--
-- * An 'MVar' holding the current Haskell value (fast reads)
-- * A reference to the connection pool (for event logging and checkpoints)
-- * A checkpoint function that writes the full state inside a transaction
data AcidState st = AcidState
  { pgMVar       :: !(MVar st)
  , pgConn       :: !PgConnection
  , pgCheckpoint :: !(st -> IO ())
    -- ^ Write the full state to checkpoint\/state tables.
    -- This runs the feature's save function inside 'runPgTx'.
  }


-- * Construction

-- | Create an 'AcidState'. The save function returns 'PgTx' and
-- will be run inside 'runPgTx' automatically on checkpoint.
mkAcidState :: PgConnection -> st -> (st -> PgTx ()) -> IO (AcidState st)
mkAcidState pool initial saveTx = do
    mvar <- newMVar initial
    return AcidState
      { pgMVar       = mvar
      , pgConn       = pool
      , pgCheckpoint = \st -> runPgTx pool (saveTx st)
      }


-- * Queries and updates

-- | Run a query (read) against the in-memory state. No database access.
queryPg :: AcidState st -> Query st a -> IO a
queryPg pgSt q = do
    st <- readMVar (pgMVar pgSt)
    return $! runReader q st

-- | Run an update against the in-memory state.
-- The caller is responsible for durable event logging (see 'updateState'
-- in "Distribution.Server.Framework.Feature").
updatePg :: AcidState st -> Update st a -> IO a
updatePg pgSt u =
    modifyMVar (pgMVar pgSt) $ \st -> do
      let (a, st') = State.runState u st
      return (st', a)


-- * Checkpoints and closing

-- | Create a checkpoint: write the full state to checkpoint\/state tables.
-- This is called periodically and on shutdown to speed up future startups
-- (so we don't have to replay the entire event log).
createCheckpoint :: AcidState st -> IO ()
createCheckpoint pgSt = do
    st <- readMVar (pgMVar pgSt)
    pgCheckpoint pgSt st

-- | Close the state: create a final checkpoint.
closeAcidState :: AcidState st -> IO ()
closeAcidState = createCheckpoint
