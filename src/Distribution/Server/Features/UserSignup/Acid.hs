{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.UserSignup.Acid where

import Distribution.Server.Features.UserSignup.Types

import Distribution.Server.Framework.MemSize

import Distribution.Server.Util.Nonce

import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy (base, deriveSafeCopy)

-------------------------
-- Types of stored data
--

newtype SignupResetTable = SignupResetTable (Map Nonce SignupResetInfo)
  deriving stock (Eq, Show)
  deriving newtype (MemSize)

emptySignupResetTable :: SignupResetTable
emptySignupResetTable = SignupResetTable Map.empty

$(deriveSafeCopy 0 'base ''SignupResetTable)
