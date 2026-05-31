{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Users.State where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Server.Users.Group (UserIdSet)
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Users.Users as Users

import Data.SafeCopy (base, deriveSafeCopy)

initialUsers :: Users.Users
initialUsers = Users.emptyUsers

-----------------------------------------------------

data HackageAdmins = HackageAdmins {
    adminList :: !Group.UserIdSet
} deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''HackageAdmins)

instance MemSize HackageAdmins where
    memSize (HackageAdmins a) = memSize1 a

initialHackageAdmins :: HackageAdmins
initialHackageAdmins = HackageAdmins Group.empty

--------------------------------------------------------------------------
data MirrorClients = MirrorClients {
    mirrorClients :: !Group.UserIdSet
} deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''MirrorClients)

instance MemSize MirrorClients where
    memSize (MirrorClients a) = memSize1 a

initialMirrorClients :: MirrorClients
initialMirrorClients = MirrorClients Group.empty
