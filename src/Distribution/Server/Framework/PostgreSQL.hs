{-# LANGUAGE OverloadedStrings #-}
-- | PostgreSQL persistence layer for hackage-server.
--
-- All database access goes through a connection 'Pool' from
-- @resource-pool@, ensuring thread safety under concurrent requests.
--
-- Transactional code uses the 'PgTx' monad (from
-- "Distribution.Server.Framework.PgTx") to enforce that all operations
-- within a transaction use the same connection.
module Distribution.Server.Framework.PostgreSQL
  ( -- * Connection pool
    PgConnection
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
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Pool (Pool, newPool, withResource, defaultPoolConfig)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import Database.PostgreSQL.Simple.Transaction (withTransactionModeRetry,
    TransactionMode(..), IsolationLevel(..), ReadWriteMode(..),
    isSerializationError)

import Database.Beam.Postgres (Pg, runBeamPostgres)

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
