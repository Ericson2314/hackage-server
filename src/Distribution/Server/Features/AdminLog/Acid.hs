{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.AdminLog.Acid where

import Distribution.Server.Features.AdminLog.Types
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework.MemSize
import Data.SafeCopy (base, deriveSafeCopy)

import Data.Time (UTCTime)
import qualified Data.ByteString.Lazy.Char8 as BS

newtype AdminLog = AdminLog {
      adminLog :: [(UTCTime,UserId,AdminAction,BS.ByteString)]
} deriving stock (Show)
  deriving newtype (MemSize)

deriveSafeCopy 0 'base ''AdminLog

initialAdminLog :: AdminLog
initialAdminLog = AdminLog []

instance Eq AdminLog where
    (AdminLog (x:_)) == (AdminLog (y:_)) = x == y
    (AdminLog []) == (AdminLog []) = True
    _ == _ = False
