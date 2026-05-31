{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.Tags.State where

import Distribution.Server.Features.Tags.Types

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Package

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Maybe (fromMaybe)
import Data.List (find, foldl')
import Control.DeepSeq

data PackageTags = PackageTags {
    -- the primary index
    packageTags :: Map PackageName (Set Tag),
    -- a secondary reverse mapping
    tagPackages :: Map Tag (Set PackageName),
    -- Packagename (Proposed Additions, Proposed Deletions)
    reviewTags :: Map PackageName (Set Tag, Set Tag)
} deriving (Eq, Show)


data TagAlias = TagAlias (Map Tag (Set Tag)) deriving (Eq, Show)

emptyPackageTags :: PackageTags
emptyPackageTags = PackageTags Map.empty Map.empty Map.empty

emptyTagAlias :: TagAlias
emptyTagAlias = TagAlias Map.empty

alterTags :: PackageName -> Maybe (Set Tag) -> PackageTags -> PackageTags
alterTags name mtagList pt@(PackageTags tags packages _) =
    let tagList = fromMaybe Set.empty mtagList
        oldTags = Map.findWithDefault Set.empty name tags
        adjustPlusTags pkgMap tag' = addSetMap tag' name pkgMap
        adjustMinusTags pkgMap tag' = removeSetMap tag' name pkgMap
        packages' = flip (foldl' adjustPlusTags) (Set.toList $ Set.difference tagList oldTags)
                  $ foldl' adjustMinusTags packages (Set.toList $ Set.difference oldTags tagList)
    in pt{
        packageTags = Map.alter (const mtagList) name tags,
        tagPackages = packages'
    }

setTags :: PackageName -> Set Tag -> PackageTags -> PackageTags
setTags pkgname tagList = alterTags pkgname (keepSet tagList)

setAliases :: Tag -> Set Tag -> TagAlias -> TagAlias
setAliases tag aliases (TagAlias ta) = TagAlias (Map.insertWith Set.union tag aliases ta)

addTag :: PackageName -> Tag -> PackageTags -> Maybe PackageTags
addTag name tag (PackageTags tags packages review) =
    let existing = Map.findWithDefault Set.empty name tags
    in if tag `Set.member` existing then Nothing else Just $ PackageTags (addSetMap name tag tags)
                                   (addSetMap tag name packages)
                                   review

removeTag :: PackageName -> Tag -> PackageTags -> Maybe PackageTags
removeTag name tag (PackageTags tags packages review) =
    let existing = Map.findWithDefault Set.empty name tags
    in if tag `Set.member` existing then Just $ PackageTags (removeSetMap name tag tags)
                                  (removeSetMap tag name packages)
                                  review else Nothing

addSetMap :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
addSetMap key val = Map.alter (Just . Set.insert val . fromMaybe Set.empty) key

removeSetMap :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
removeSetMap key val = Map.update (keepSet . Set.delete val) key

alterTag :: Tag -> Maybe (Set PackageName) -> PackageTags -> PackageTags
alterTag tag mpkgList (PackageTags tags packages review) =
    let pkgList = fromMaybe Set.empty mpkgList
        oldPkgs = Map.findWithDefault Set.empty tag packages
        adjustPlusPkgs tagMap name' = addSetMap name' tag tagMap
        adjustMinusPkgs tagMap name' = removeSetMap name' tag tagMap
        tags' = flip (foldl' adjustPlusPkgs) (Set.toList $ Set.difference pkgList oldPkgs)
              $ foldl' adjustMinusPkgs tags (Set.toList $ Set.difference oldPkgs pkgList)
    in PackageTags tags' (Map.alter (const mpkgList) tag packages) review

keepSet :: Ord a => Set a -> Maybe (Set a)
keepSet s = if Set.null s then Nothing else Just s

-- these three are not currently exposed as happstack-state functions
setTag :: Tag -> Set PackageName -> PackageTags -> PackageTags
setTag tag pkgs = alterTag tag (keepSet pkgs)

-------------------------------------------------------------------------------

$(deriveSafeCopy 0 'base ''PackageTags)
$(deriveSafeCopy 0 'base ''TagAlias)

instance NFData PackageTags where
    rnf (PackageTags a b c) = rnf a `seq` rnf b `seq` rnf c

instance MemSize PackageTags where
    memSize (PackageTags a b c) = memSize3 a b c

initialPackageTags :: PackageTags
initialPackageTags = emptyPackageTags

-- | Look up tag for an alias — pure function used by db layer
getTagAliasValue :: Tag -> TagAlias -> Tag
getTagAliasValue tag (TagAlias m) =
  if Map.member tag m then tag
  else maybe tag fst $ find (Set.member tag . snd) $ Map.toList m

insertReviewHelper :: (Set Tag, Set Tag) -> (Set Tag, Set Tag) -> (Set Tag, Set Tag)
insertReviewHelper (a,b) (c,d) = (Set.union a c, Set.union b d)

