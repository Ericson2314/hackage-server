{-# LANGUAGE DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Implements a system to allow users to upvote packages.
-- De-event-sourced: reads/writes go directly to PostgreSQL.
module Distribution.Server.Features.Votes
  ( VotesFeature(..)
  , initVotesFeature
  ) where

import Distribution.Server.Features.Votes.Types (Score)
import qualified Distribution.Server.Features.Votes.State as State
import qualified Distribution.Server.Features.Votes.Render as Render

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users
import Distribution.Server.Users.Types (UserId(..))

import Distribution.Package
import Distribution.Text

import Data.Aeson
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Arrow (first)
import qualified Text.XHtml.Strict as X

import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Data.List (foldl')
import Data.Int (Int32)


-- | Define the prototype for this feature
data VotesFeature = VotesFeature {
    votesFeatureInterface :: HackageFeature
  , didUserVote             :: forall m. MonadIO m => PackageName -> UserId -> m Bool
  , pkgNumVotes             :: forall m. MonadIO m => PackageName -> m Int
  , pkgNumScore             :: forall m. MonadIO m => PackageName -> m Float
  , pkgUserVote             :: forall m. MonadIO m => PackageName -> UserId -> m (Maybe Score)
  , votesUpdated            :: Hook (PackageName, Float) ()
  , renderVotesHtml         :: PackageName -> ServerPartE X.Html
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature VotesFeature where
  getFeatureInterface = votesFeatureInterface

-- | Called from Features.hs to initialize this feature
initVotesFeature :: ServerEnv
                   -> IO ( CoreFeature
                      -> UserFeature
                      -> IO VotesFeature)
initVotesFeature ServerEnv{serverPgConn} = do
  updateVotes <- newHook

  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = votesFeature serverPgConn
                  coref userf updateVotes
    return feature

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
  VotesDb (setEntityName "votes__votes" <>
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
    return $ if Map.null userScores then 0 else State.votesScore userScores

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
dbPutAllVotes :: PgConnection -> State.VotesState -> IO ()
dbPutAllVotes pool (State.VotesState votes) =
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
-- Feature
--

votesFeature :: PgConnection
             -> CoreFeature                    -- To get site package list
             -> UserFeature                    -- To authenticate users
             -> Hook (PackageName, Float) ()
             -> VotesFeature

votesFeature pool
             CoreFeature { coreResource = CoreResource{..} }
             UserFeature{..}
             votesUpdated
  = VotesFeature{..}
  where
    votesFeatureInterface = (emptyHackageFeature "votes") {
        featureDesc      = "Allow users to upvote packages",
        featureResources = [ packagesVotesResource
                           , packageVotesResource
                           ]
      }

    -- Define resources for this feature's URIs

    packagesVotesResource :: Resource
    packagesVotesResource = (resourceAt "/packages/votes.:format") {
      resourceDesc  = [(GET,    "Returns the number of votes for each package")]
    , resourceGet   = [("json", servePackageVotesGet)]
    }

    packageVotesResource :: Resource
    packageVotesResource = (resourceAt "/package/:package/votes.:format") {
      resourceDesc    = [ (GET,     "Returns the number of votes a package has")
                        , (PUT,     "Adds a vote to this package")
                        , (DELETE,  "Remove a user's vote from this package")
                        ]
    , resourceGet     = [("json", servePackageNumVotesGet)]
    , resourcePost    = [("",     servePackageVotePut)]
    , resourceDelete  = [("",     servePackageVoteDelete)]
    }

    -- Implementations of the how the above resources are handled.

    -- Retrive the entire map (from package names -> # of votes)
    servePackageVotesGet :: DynamicPath -> ServerPartE Response
    servePackageVotesGet _ = do
      cacheControlWithoutETag [Public, maxAgeMinutes 10]
      allVotes <- liftIO $ dbGetAllVotes pool
      ok . toResponse $ objectL
        [ (display pkgname, toJSON (State.votesScore pkgMap))
        | (pkgname, pkgMap) <- Map.toList allVotes ]

    -- Get the number of votes a package has. If the package
    -- has never been voted for, returns 0.
    servePackageNumVotesGet :: DynamicPath -> ServerPartE Response
    servePackageNumVotesGet dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      cacheControlWithoutETag [Public, maxAgeMinutes 10]
      voteCount <- pkgNumVotes pkgname
      ok . toResponse $ objectL
        [ ("packageName", string $ display pkgname)
        , ("numVotes",    toJSON voteCount)
        ]

    -- Add a vote to :packageName (must match name exactly)
    servePackageVotePut :: DynamicPath -> ServerPartE Response
    servePackageVotePut dpath = do
      uid     <- guardAuthorised [AnyKnownUser]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      scoreStr <- look "score"
      -- very simple input validation; we accept only three literals
      score <- case scoreStr of
        "1" -> pure 1
        "2" -> pure 2
        "3" -> pure 3
        _   -> fail "invalid score value received"
      liftIO $ dbAddVote pool pkgname uid score
      pkgScore <- pkgNumScore pkgname
      runHook_ votesUpdated (pkgname, pkgScore)
      ok . toResponse $ "Package voted for successfully"

    -- Removes a user's vote from a package. If the user has not voted
    -- for this package, does nothing.
    servePackageVoteDelete :: DynamicPath -> ServerPartE Response
    servePackageVoteDelete dpath = do
      uid     <- guardAuthorised [AnyKnownUser]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      success <- liftIO $ dbRemoveVote pool pkgname uid
      pkgScore <- pkgNumScore pkgname
      when success $ runHook_ votesUpdated (pkgname, pkgScore)
      let responseMsg | success   = "Package vote removed successfully."
                      | otherwise = "User has not voted for this package."
      ok . toResponse $ responseMsg

    -- Helper Functions (Used outside of responses, e.g. by other features.)

    -- Returns true if a user has previously voted for the
    -- package in question.
    didUserVote :: MonadIO m => PackageName -> UserId -> m Bool
    didUserVote pkgname uid = liftIO $ dbDidUserVote pool pkgname uid

    -- Returns the number of votes a package has.
    pkgNumVotes :: MonadIO m => PackageName -> m Int
    pkgNumVotes pkgname = liftIO $ dbPkgNumVotes pool pkgname

    pkgNumScore :: MonadIO m => PackageName -> m Float
    pkgNumScore pkgname = liftIO $ dbPkgScore pool pkgname

    pkgUserVote :: MonadIO m => PackageName -> UserId -> m (Maybe Score)
    pkgUserVote pkgname uid = liftIO $ dbPkgUserVote pool pkgname uid

    -- Renders the HTML for the "Votes:" section on package pages.
    renderVotesHtml :: PackageName -> ServerPartE X.Html
    renderVotesHtml pkgname = do
      numVotes <- pkgNumVotes pkgname
      return $ Render.renderVotesAnon numVotes pkgname


-- Helper functions for constructing JSON responses.

-- Use to construct a list of tuples that can be toJSON'd
objectL :: [(String, Value)] -> Value
objectL = Object . KeyMap.fromList . map (first Key.fromString)

-- Use inside an objectL to transform strings into json values
string :: String -> Value
string = String . T.pack
