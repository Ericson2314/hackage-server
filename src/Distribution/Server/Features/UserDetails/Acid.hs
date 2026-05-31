{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.UserDetails.Acid where

import Distribution.Server.Features.UserDetails.Types
import Distribution.Server.Framework.MemSize

import Data.SafeCopy (base, deriveSafeCopy)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Text as T

-------------------------
-- Types of stored data
--

newtype UserDetailsTable = UserDetailsTable (IntMap AccountDetails)
  deriving (Eq, Show)

emptyAccountDetails :: AccountDetails
emptyAccountDetails   = AccountDetails T.empty T.empty Nothing T.empty

emptyUserDetailsTable :: UserDetailsTable
emptyUserDetailsTable = UserDetailsTable IntMap.empty

$(deriveSafeCopy 0 'base ''UserDetailsTable)

instance MemSize UserDetailsTable where
    memSize (UserDetailsTable a) = memSize1 a

