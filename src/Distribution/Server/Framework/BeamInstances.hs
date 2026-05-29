{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Beam instances for types we can't modify (Cabal library types),
-- generic collection instances, and PostgreSQL composite type helpers.
-- All other types define their own beam instances to avoid orphans and
-- import cycles.
module Distribution.Server.Framework.BeamInstances
  ( -- * PostgreSQL composite types for build reports
    PgFlagAssignment(..)
  , PgPackageIdentifier(..)
  ) where

import Control.Applicative ((<|>), many)
import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Attoparsec.ByteString.Char8 as A

import Database.Beam.Backend (FromBackendRow(..))
import Database.Beam.Backend.SQL (HasSqlValueSyntax(..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)

import Database.PostgreSQL.Simple.FromField (FromField(..), typename, returnError, ResultError(..))
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))

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

------------------------------------------------------------------------
-- PostgreSQL composite types for build reports
--
-- These match CREATE TYPE flag_assignment and CREATE TYPE package_identifier
-- in schema.sql.

-- | Corresponds to @CREATE TYPE flag_assignment AS (flag_name TEXT, flag_value BOOL)@
data PgFlagAssignment = PgFlagAssignment
  { pgFlagName  :: !T.Text
  , pgFlagValue :: !Bool
  } deriving (Eq, Show, Typeable)

instance FromField PgFlagAssignment where
  fromField f mbs = do
    tname <- typename f
    if tname /= "flag_assignment"
      then returnError Incompatible f ""
      else case mbs of
        Nothing -> returnError UnexpectedNull f ""
        Just bs -> case A.parseOnly parseFlagAssignment bs of
          Left err -> returnError ConversionFailed f err
          Right v  -> pure v

parseFlagAssignment :: A.Parser PgFlagAssignment
parseFlagAssignment = do
  _ <- A.char '('
  name <- pgTextContent
  _ <- A.char ','
  val <- (True <$ A.char 't') <|> (False <$ A.char 'f')
  _ <- A.char ')'
  pure PgFlagAssignment { pgFlagName = name, pgFlagValue = val }

instance ToField PgFlagAssignment where
  toField PgFlagAssignment{..} =
    Many [ Plain "("
         , Escape (T.encodeUtf8 pgFlagName)
         , Plain ","
         , Plain (if pgFlagValue then "t" else "f")
         , Plain ")"
         ]

-- | Corresponds to @CREATE TYPE package_identifier AS (pkg_name TEXT, pkg_version TEXT)@
data PgPackageIdentifier = PgPackageIdentifier
  { pgPkgName    :: !T.Text
  , pgPkgVersion :: !T.Text
  } deriving (Eq, Show, Typeable)

instance FromField PgPackageIdentifier where
  fromField f mbs = do
    tname <- typename f
    if tname /= "package_identifier"
      then returnError Incompatible f ""
      else case mbs of
        Nothing -> returnError UnexpectedNull f ""
        Just bs -> case A.parseOnly parsePackageIdentifier bs of
          Left err -> returnError ConversionFailed f err
          Right v  -> pure v

parsePackageIdentifier :: A.Parser PgPackageIdentifier
parsePackageIdentifier = do
  _ <- A.char '('
  name <- pgTextContent
  _ <- A.char ','
  ver <- pgTextContent
  _ <- A.char ')'
  pure PgPackageIdentifier { pgPkgName = name, pgPkgVersion = ver }

instance ToField PgPackageIdentifier where
  toField PgPackageIdentifier{..} =
    Many [ Plain "("
         , Escape (T.encodeUtf8 pgPkgName)
         , Plain ","
         , Escape (T.encodeUtf8 pgPkgVersion)
         , Plain ")"
         ]

-- | Parse a text field inside a composite value — either quoted or plain.
pgTextContent :: A.Parser T.Text
pgTextContent = T.decodeLatin1 <$> (pgQuoted <|> pgPlain)

pgQuoted :: A.Parser B.ByteString
pgQuoted = A.char '"' *> (mconcat <$> many chunk) <* A.char '"'
  where
    esc   = A.char '\\' *> (A.char '\\' <|> A.char '"')
    unQ   = A.takeWhile1 (A.notInClass "\"\\")
    chunk = unQ <|> (B.singleton <$> esc)

pgPlain :: A.Parser B.ByteString
pgPlain = A.takeWhile1 (A.notInClass ",\"()")
