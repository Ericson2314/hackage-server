{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.Vouch.State where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (UTCTime)

import Distribution.Server.Framework (MemSize(..), memSize2)
import Data.SafeCopy (base, deriveSafeCopy)
import Distribution.Server.Users.Types (UserId)

data VouchData =
  VouchData
    { vouches :: Map.Map UserId [(UserId, UTCTime)]
    , notNotified :: Set.Set UserId
    }
  deriving (Show, Eq)

instance MemSize VouchData where
  memSize (VouchData vouches notified) = memSize2 vouches notified

$(deriveSafeCopy 0 'base ''VouchData)
