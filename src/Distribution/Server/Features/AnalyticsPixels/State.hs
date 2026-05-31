{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.AnalyticsPixels.State
    ( AnalyticsPixelsState(..)
    , initialAnalyticsPixelsState
    ) where

import Distribution.Server.Features.AnalyticsPixels.Types
import Distribution.Package (PackageName)

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize (MemSize)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)

import Control.DeepSeq (NFData)

newtype AnalyticsPixelsState = AnalyticsPixelsState
    {
        analyticsPixels :: Map PackageName (Set AnalyticsPixel)
    }
  deriving stock (Show, Eq)
  deriving newtype (NFData, MemSize)

-- SafeCopy instances
$(deriveSafeCopy 0 'base ''AnalyticsPixel)
$(deriveSafeCopy 0 'base ''AnalyticsPixelsState)

initialAnalyticsPixelsState :: AnalyticsPixelsState
initialAnalyticsPixelsState = AnalyticsPixelsState
    {
        analyticsPixels = Map.empty
    }
