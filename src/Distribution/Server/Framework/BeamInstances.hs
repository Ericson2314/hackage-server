{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Beam instances for types we can't modify (Cabal library types)
-- and generic collection instances. All other types define their own
-- beam instances to avoid orphans and import cycles.
module Distribution.Server.Framework.BeamInstances () where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Text as T

import Database.Beam.Backend (FromBackendRow(..))
import Database.Beam.Backend.SQL (HasSqlValueSyntax(..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)

import Distribution.Package (PackageName, PackageIdentifier)
import Distribution.Version (Version, VersionRange)
import Distribution.Text (display, simpleParse)

-- Cabal types (external library, can't add instances there)
instance HasSqlValueSyntax PgValueSyntax PackageName where sqlValueSyntax = sqlValueSyntax . T.pack . display
instance FromBackendRow Postgres PackageName where
  fromBackendRow = do { t <- fromBackendRow; case simpleParse (T.unpack (t :: T.Text)) of Just v -> pure v; Nothing -> fail "Invalid PackageName" }
instance HasSqlValueSyntax PgValueSyntax Version where sqlValueSyntax = sqlValueSyntax . T.pack . display
instance FromBackendRow Postgres Version where
  fromBackendRow = do { t <- fromBackendRow; case simpleParse (T.unpack (t :: T.Text)) of Just v -> pure v; Nothing -> fail "Invalid Version" }
instance HasSqlValueSyntax PgValueSyntax PackageIdentifier where sqlValueSyntax = sqlValueSyntax . T.pack . display
instance FromBackendRow Postgres PackageIdentifier where
  fromBackendRow = do { t <- fromBackendRow; case simpleParse (T.unpack (t :: T.Text)) of Just v -> pure v; Nothing -> fail "Invalid PackageIdentifier" }
instance HasSqlValueSyntax PgValueSyntax VersionRange where sqlValueSyntax = sqlValueSyntax . T.pack . show

-- Generic collections
instance (Show a, Ord a) => HasSqlValueSyntax PgValueSyntax (Set a) where sqlValueSyntax = sqlValueSyntax . T.pack . show
instance (Read a, Ord a) => FromBackendRow Postgres (Set a) where
  fromBackendRow = do { t <- fromBackendRow; pure $ read (T.unpack (t :: T.Text)) }
instance (Show k, Show v) => HasSqlValueSyntax PgValueSyntax (Map k v) where sqlValueSyntax = sqlValueSyntax . T.pack . show
instance (Show a) => HasSqlValueSyntax PgValueSyntax [a] where sqlValueSyntax = sqlValueSyntax . T.pack . show
instance (Read a) => FromBackendRow Postgres [a] where
  fromBackendRow = do { t <- fromBackendRow; pure $ read (T.unpack (t :: T.Text)) }
