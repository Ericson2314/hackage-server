{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.PreferredVersions.State where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Package
import Distribution.Version

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.SafeCopy (Migrate(..), base, extension, deriveSafeCopy)

data PreferredVersions = PreferredVersions {
    preferredMap  :: Map PackageName PreferredInfo,
    deprecatedMap :: Map PackageName [PackageName],
    migratedEphemeralPrefs :: Bool
} deriving (Show, Eq)


-- NOTE: preferred versions no longer exist; this structure is actually only
-- used to keep around 'deprecatedVersions'.
--
-- The unused fields are kept around so as to not change the
-- automatically-derived serialization format.
data PreferredInfo = PreferredInfo {
    unused_preferredRanges :: [VersionRange],
    deprecatedVersions :: [Version],
    -- | Use 'sumRange' instead.
    unused_sumRange :: Maybe VersionRange
} deriving (Show, Eq)

{-# DEPRECATED
      unused_preferredRanges
      "This field is completely unused, but is kept around to not change the automatically derived serialization format." #-}
{-# DEPRECATED
      unused_sumRange
      "This field is completely unused, but is kept around to not change the automatically derived serialization format." #-}

emptyPreferredInfo :: PreferredInfo
emptyPreferredInfo = PreferredInfo [] [] Nothing


sumRange :: PreferredInfo -> Maybe VersionRange
sumRange (PreferredInfo ranges depr _) =
    let range = simplifyVersionRange $ foldr intersectVersionRanges anyVersion (map notThisVersion depr ++ ranges)
    in if isAnyVersion range || isNoVersion range
        then Nothing
        else Just range


data PreferredVersions_v0
   = PreferredVersions_v0 (Map PackageName PreferredInfo)
                          (Map PackageName [PackageName])

$(deriveSafeCopy 0 'base ''PreferredInfo)
$(deriveSafeCopy 0 'base ''PreferredVersions_v0)

instance Migrate PreferredVersions where
    type MigrateFrom PreferredVersions = PreferredVersions_v0
    migrate (PreferredVersions_v0 prefs deprs) =
      PreferredVersions {
        preferredMap  = prefs,
        deprecatedMap = deprs,
        migratedEphemeralPrefs = False
      }

------------------------------------------
$(deriveSafeCopy 1 'extension ''PreferredVersions)

instance MemSize PreferredVersions where
    memSize (PreferredVersions a b c) = memSize3 a b c

instance MemSize PreferredInfo where
    memSize (PreferredInfo a b c) = memSize3 a b c

-- | Initial PreferredVersions
initialPreferredVersions :: Bool -> PreferredVersions
initialPreferredVersions freshDB = PreferredVersions {
    preferredMap           = Map.empty
  , deprecatedMap          = Map.empty
  , migratedEphemeralPrefs = freshDB
  }

