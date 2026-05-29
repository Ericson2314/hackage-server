{-# LANGUAGE DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Votes.Acid
  ( acidStore
  , votesStateComponent
  ) where

import Distribution.Server.Features.Votes.Store
import qualified Distribution.Server.Features.Votes.State as Acid

import Distribution.Server.Framework
import Distribution.Server.Users.Types (UserId(..))

import Distribution.Text

import qualified Data.Map as Map
import qualified Data.Text as T

import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)
import Data.List (foldl')
import Data.Int (Int32)

acidStore :: PgConnection -> IO Backend
acidStore pgConn = do
  votesState <- votesStateComponent pgConn
  return Backend {
    backendStore = Store {
        getAllPackageVoteScores = Map.map votesScore <$> queryState votesState Acid.GetAllPackageVoteSets
      , addVote               = \pkgname uid score -> updateState votesState (Acid.AddVote pkgname uid score)
      , removeVote            = \pkgname uid -> updateState votesState (Acid.RemoveVote pkgname uid)
      , getPackageVoteCount   = \pkgname -> queryState votesState (Acid.GetPackageVoteCount pkgname)
      , getPackageVoteScore   = \pkgname -> queryState votesState (Acid.GetPackageVoteScore pkgname)
      , getPackageUserVoted   = \pkgname uid -> queryState votesState (Acid.GetPackageUserVoted pkgname uid)
      , getPackageUserVote    = \pkgname uid -> queryState votesState (Acid.GetPackageUserVote pkgname uid)
      }
    , backendState = [abstractAcidStateComponent votesState]
    }

------------------------------------------------------------------------
-- Beam table
--

data VoteRowT f = VoteRow
  { _vrPkgName :: C f T.Text
  , _vrUserId  :: C f Int32
  , _vrScore   :: C f Int32
  } deriving (Generic, Beamable)

instance Table VoteRowT where
  data PrimaryKey VoteRowT f =
    VoteRowId (C f T.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = VoteRowId (_vrPkgName r) (_vrUserId r)

deriving instance Show (VoteRowT Identity)

data VotesDb f = VotesDb
  { _votesRows :: f (TableEntity VoteRowT)
  } deriving (Generic, Database Postgres)

votesDb :: DatabaseSettings Postgres VotesDb
votesDb = defaultDbSettings `withDbModification`
  VotesDb (setEntityName "votes" <>
           modifyTableFields tableModification
             { _vrPkgName = "pkg_name"
             , _vrUserId  = "user_id"
             , _vrScore   = "score"
             })

votesTable :: DatabaseEntity Postgres VotesDb (TableEntity VoteRowT)
votesTable = _votesRows votesDb

loadVotesState :: PgTx Acid.VotesState
loadVotesState = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ votesTable
  let addRow m (VoteRow name uid score) =
        case simpleParse (T.unpack name) of
          Just pkgName ->
            Map.insertWith Map.union pkgName
              (Map.singleton (UserId (fromIntegral uid)) (fromIntegral score)) m
          Nothing -> m
  return $ Acid.VotesState $ foldl' addRow Map.empty rows

saveVotesState :: Acid.VotesState -> PgTx ()
saveVotesState (Acid.VotesState votes) =
  do
    beamTx $
      runDelete $ delete votesTable (\_ -> val_ True)
    let rows = [ VoteRow (T.pack $ display pkgName) (fromIntegral uid) (fromIntegral score)
               | (pkgName, userMap) <- Map.toList votes
               , (UserId uid, score) <- Map.toList userMap ]
    mapM_ (insertVoteChunk) (chunksOf 1000 rows)

insertVoteChunk :: [VoteRowT Identity] -> PgTx ()
insertVoteChunk chunk =
  beamTx $
    runInsert $ insert votesTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

-- | Define the backing store (i.e. database component)
votesStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.VotesState)
votesStateComponent serverPgConn = do
  -- Load state
  loaded <- runPgTx serverPgConn loadVotesState

  pgSt <- mkAcidState serverPgConn loaded saveVotesState
  return StateComponent {
      stateDesc    = "Backing store for Map PackageName -> Users who voted for it"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetVotesState)
    , putState     = \s -> do
        runPgTx serverPgConn (saveVotesState s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }


