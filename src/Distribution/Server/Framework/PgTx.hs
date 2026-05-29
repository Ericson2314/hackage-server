{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Transaction monad for PostgreSQL operations.
--
-- 'PgTx' is a 'ReaderT' over a single 'PG.Connection', ensuring all
-- operations within a transaction use the same connection. The type
-- system prevents accidentally using the connection pool inside a
-- transaction.
--
-- Use 'beamTx' for beam queries and 'pgTxRaw' for raw
-- postgresql-simple operations.
module Distribution.Server.Framework.PgTx
  ( PgTx(..)  -- constructor exported for runPgTx in PostgreSQL module
  , beamTx
  , pgTxRaw
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), ask)
import qualified Database.PostgreSQL.Simple as PG
import Database.Beam.Postgres (Pg, runBeamPostgres)

-- | A monad for transactional database operations.
-- All operations use the same connection (from the reader).
newtype PgTx a = PgTx (ReaderT PG.Connection IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a beam query inside the transaction.
beamTx :: Pg a -> PgTx a
beamTx action = PgTx $ do
  conn <- ask
  liftIO $ runBeamPostgres conn action

-- | Run a raw postgresql-simple operation inside the transaction.
pgTxRaw :: (PG.Connection -> IO a) -> PgTx a
pgTxRaw action = PgTx $ do
  conn <- ask
  liftIO $ action conn
