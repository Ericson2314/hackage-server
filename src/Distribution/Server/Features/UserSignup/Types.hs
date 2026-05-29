{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.UserSignup.Types where

import Database.Beam.Backend.SQL (HasSqlValueSyntax(..))
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import qualified Data.Text as T
import Distribution.Server.Framework

import Distribution.Server.Users.Types

import Data.Text (Text)
import Data.SafeCopy (base, deriveSafeCopy)

import Data.Time

-------------------------
-- Types of stored data
--

data SignupResetInfo = SignupInfo {
                         signupUserName     :: !Text,
                         signupRealName     :: !Text,
                         signupContactEmail :: !Text,
                         nonceTimestamp     :: !UTCTime
                       }
                     | ResetInfo {
                         resetUserId        :: !UserId,
                         nonceTimestamp     :: !UTCTime
                     }
  deriving (Eq, Show)

instance MemSize SignupResetInfo where
    memSize (SignupInfo a b c d) = memSize4 a b c d
    memSize (ResetInfo  a b)     = memSize2 a b

$(deriveSafeCopy 0 'base ''SignupResetInfo)

instance HasSqlValueSyntax PgValueSyntax SignupResetInfo where sqlValueSyntax = sqlValueSyntax . T.pack . show
