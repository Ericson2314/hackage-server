{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Distribution.Server.Features.AnalyticsPixels.Types
    ( AnalyticsPixel(..)
    ) where

import Distribution.Server.Framework.MemSize (MemSize)
import Database.Beam.Backend.SQL (HasSqlValueSyntax(..))
import Database.Beam.Postgres.Syntax (PgValueSyntax)

import Data.Text (Text)

import Control.DeepSeq (NFData)

newtype AnalyticsPixel = AnalyticsPixel
    {
        analyticsPixelUrl :: Text
    }
    deriving (Show, Eq, Ord, NFData, MemSize)

-- Beam stores as TEXT via the underlying Text
instance HasSqlValueSyntax PgValueSyntax AnalyticsPixel where
  sqlValueSyntax (AnalyticsPixel url) = sqlValueSyntax url
