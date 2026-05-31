{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Votes state type — kept for backup/restore compatibility.
-- The live server reads/writes directly from PostgreSQL.
module Distribution.Server.Features.Votes.State where

import Distribution.Server.Features.Votes.Types
import Distribution.Server.Framework.MemSize

import Distribution.Package (PackageName)
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Users.UserIdSet (UserIdSet)
import qualified Distribution.Server.Users.UserIdSet as UserIdSet

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.SafeCopy (base, extension, deriveSafeCopy, Migrate(..))

-- | In-memory representation, used only for backup/restore.
newtype VotesState = VotesState { unVotesState :: Map PackageName (Map UserId Score) }
  deriving stock (Show, Eq)
  deriving newtype (MemSize)

newtype VotesState_v0 = VotesState_v0 { votesMap :: Map PackageName UserIdSet }

deriveSafeCopy 0 'base      ''VotesState_v0
deriveSafeCopy 1 'extension ''VotesState

instance Migrate VotesState where
    type MigrateFrom VotesState = VotesState_v0
    migrate (VotesState_v0 m) = VotesState (Map.map go m)
      where
        go :: UserIdSet -> Map UserId Score
        go = Map.fromList . map (\x->(x,3)) . UserIdSet.toList

initialVotesState :: VotesState
initialVotesState = VotesState Map.empty

-- | Bayesian average scoring
votesScore :: Map UserId Score -> Float
votesScore m =
     let grouping = map (\g -> (head g, fromIntegral (length g) :: Score)) . group . sort . Map.elems $ m
         score :: Float
         score = fromIntegral ((sum $ map (uncurry (*)) grouping) + 3)/
                 fromIntegral (2 + sum (map snd grouping))
         roundedScore = fromIntegral (round (score * 4) :: Int) / 4
     in roundedScore
