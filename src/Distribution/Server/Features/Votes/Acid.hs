{-# LANGUAGE DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Votes.Acid
  ( acidStore
  ) where

import Distribution.Server.Features.Votes.Store
import qualified Distribution.Server.Features.Votes.State as Acid

import Distribution.Server.Framework
import Distribution.Server.Users.Types (UserId(..))

import Distribution.Text
import Distribution.Package (PackageName)
import Distribution.Server.Features.Votes.Types (Score)
import Control.Monad (when)

import qualified Data.Map as Map
import qualified Data.Text as T

import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Data.List (foldl')
import Data.Int (Int32)

acidStore :: PgConnection -> IO Backend
acidStore pool =
  return Backend {
    backendStore = Store {
        getAllPackageVoteScores = liftIO $ Map.map votesScore <$> dbGetAllVotes pool
      , addVote               = \pkgname uid score -> liftIO $ do
          dbAddVote pool pkgname uid score
          dbPkgScore pool pkgname
      , removeVote            = \pkgname uid -> liftIO $ dbRemoveVote pool pkgname uid
      , getPackageVoteCount   = \pkgname -> liftIO $ dbPkgNumVotes pool pkgname
      , getPackageVoteScore   = \pkgname -> liftIO $ dbPkgScore pool pkgname
      , getPackageUserVoted   = \pkgname uid -> liftIO $ dbDidUserVote pool pkgname uid
      , getPackageUserVote    = \pkgname uid -> liftIO $ dbPkgUserVote pool pkgname uid
      }
    , backendState = []  -- no AcidState; data lives in PostgreSQL
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

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get all votes as a map (for backup or bulk queries)
dbGetAllVotes :: PgConnection -> IO (Map.Map PackageName (Map.Map UserId Score))
dbGetAllVotes pool = runBeamPg pool $ do
  rows <- runSelectReturningList $ select $ all_ votesTable
  return $ foldl' addRow Map.empty rows
  where
    addRow m (VoteRow name uid score) =
      case simpleParse (T.unpack name) of
        Just pkgName ->
          Map.insertWith Map.union pkgName
            (Map.singleton (UserId (fromIntegral uid)) (fromIntegral score)) m
        Nothing -> m

-- | Add or update a vote
dbAddVote :: PgConnection -> PackageName -> UserId -> Score -> IO ()
dbAddVote pool pkgname (UserId uid) score =
    runPgTx pool $ do
      -- Delete existing vote if any, then insert
      beamTx $ runDelete $ delete votesTable
        (\v -> _vrPkgName v ==. val_ (T.pack $ display pkgname)
           &&. _vrUserId v  ==. val_ (fromIntegral uid))
      beamTx $ runInsert $ insert votesTable $ insertValues
        [VoteRow (T.pack $ display pkgname) (fromIntegral uid) (fromIntegral score)]

-- | Remove a vote, returns True if it existed
dbRemoveVote :: PgConnection -> PackageName -> UserId -> IO Bool
dbRemoveVote pool pkgname uid = do
    existed <- dbDidUserVote pool pkgname uid
    when existed $
      runBeamPg pool $
        runDelete $ delete votesTable
          (\v -> _vrPkgName v ==. val_ (T.pack $ display pkgname)
             &&. _vrUserId v  ==. val_ (let UserId u = uid in fromIntegral u))
    return existed

-- | Check if a user voted for a package
dbDidUserVote :: PgConnection -> PackageName -> UserId -> IO Bool
dbDidUserVote pool pkgname (UserId uid) = do
    rows <- runBeamPg pool $
      runSelectReturningList $ select $
        filter_ (\v -> _vrPkgName v ==. val_ (T.pack $ display pkgname)
                   &&. _vrUserId v  ==. val_ (fromIntegral uid)) $
        all_ votesTable
    return (not (null rows))

-- | Get number of votes for a package
dbPkgNumVotes :: PgConnection -> PackageName -> IO Int
dbPkgNumVotes pool pkgname = do
    rows <- runBeamPg pool $
      runSelectReturningList $ select $
        filter_ (\v -> _vrPkgName v ==. val_ (T.pack $ display pkgname)) $
        all_ votesTable
    return (length rows)

-- | Get score for a package
dbPkgScore :: PgConnection -> PackageName -> IO Float
dbPkgScore pool pkgname = do
    rows <- runBeamPg pool $
      runSelectReturningList $ select $
        filter_ (\v -> _vrPkgName v ==. val_ (T.pack $ display pkgname)) $
        all_ votesTable
    let userScores = Map.fromList
          [ (UserId (fromIntegral uid), fromIntegral score)
          | VoteRow _ uid score <- rows ]
    return $ if Map.null userScores then 0 else votesScore userScores

-- | Get a user's vote for a package
dbPkgUserVote :: PgConnection -> PackageName -> UserId -> IO (Maybe Score)
dbPkgUserVote pool pkgname (UserId uid) = do
    rows <- runBeamPg pool $
      runSelectReturningList $ select $
        filter_ (\v -> _vrPkgName v ==. val_ (T.pack $ display pkgname)
                   &&. _vrUserId v  ==. val_ (fromIntegral uid)) $
        all_ votesTable
    return $ case rows of
      (VoteRow _ _ score : _) -> Just (fromIntegral score)
      [] -> Nothing

-- | Write full state to DB (for backup restore)
dbPutAllVotes :: PgConnection -> Acid.VotesState -> IO ()
dbPutAllVotes pool (Acid.VotesState votes) =
    runPgTx pool $ do
      beamTx $ runDelete $ delete votesTable (\_ -> val_ True)
      let rows = [ VoteRow (T.pack $ display pkgName) (fromIntegral uid) (fromIntegral score)
                 | (pkgName, userMap) <- Map.toList votes
                 , (UserId uid, score) <- Map.toList userMap ]
      mapM_ insertChunk (chunksOf 1000 rows)
  where
    insertChunk chunk = beamTx $
      runInsert $ insert votesTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------
