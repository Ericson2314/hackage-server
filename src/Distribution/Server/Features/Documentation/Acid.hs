{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Distribution.Server.Features.Documentation.Acid
  ( acidStore
  ) where

import qualified Distribution.Server.Features.Documentation.State as State
import qualified Distribution.Server.Features.Documentation.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Framework.BlobStorage (blobMd5, readBlobId)

import Distribution.Package (PackageIdentifier(..))
import Distribution.Text (display, simpleParse)
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)

acidStore :: String -> PgConnection -> IO Store.Backend
acidStore name serverPgConn = do
  documentationState <- documentationStateComponent name serverPgConn
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.hasDocumentation = \pkgid -> queryState documentationState (State.HasDocumentation pkgid)
        , Store.lookupDocumentation = \pkgid -> queryState documentationState (State.LookupDocumentation pkgid)
        , Store.getDocumentationIndex = State.documentation <$> queryState documentationState State.GetDocumentation
        , Store.insertDocumentation = \pkgid blobid -> updateState documentationState (State.InsertDocumentation pkgid blobid)
        , Store.removeDocumentation = \pkgid -> updateState documentationState (State.RemoveDocumentation pkgid)
        }
    , Store.backendState = [abstractAcidStateComponent documentationState]
    }

------------------------------------------------------------------------
-- Beam table
--

data DocRowT f = DocRow
  { _drPkgName    :: C f T.Text
  , _drPkgVersion :: C f T.Text
  , _drBlobId     :: C f T.Text
  } deriving (Generic, Beamable)

instance Table DocRowT where
  data PrimaryKey DocRowT f =
    DocRowId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = DocRowId (_drPkgName r) (_drPkgVersion r)

deriving instance Show (DocRowT Identity)

data DocDb f = DocDb
  { _docRows :: f (TableEntity DocRowT)
  } deriving (Generic, Database Postgres)

docDb :: DatabaseSettings Postgres DocDb
docDb = defaultDbSettings `withDbModification`
  DocDb (setEntityName "documentation" <>
         modifyTableFields tableModification
           { _drPkgName    = "pkg_name"
           , _drPkgVersion = "pkg_version"
           , _drBlobId     = "blob_id"
           })

docTable :: DatabaseEntity Postgres DocDb (TableEntity DocRowT)
docTable = _docRows docDb

loadDocumentation :: PgTx State.Documentation
loadDocumentation = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ docTable
  let addRow m (DocRow name ver blobHex) =
        case (simpleParse (T.unpack name), simpleParse (T.unpack ver), readBlobId (T.unpack blobHex)) of
          (Just pkgName, Just pkgVer, Right blobId) ->
            Map.insert (PackageIdentifier pkgName pkgVer) blobId m
          _ -> m  -- skip unparseable rows
  return $ State.Documentation $ foldl' addRow Map.empty rows

saveDocumentation :: State.Documentation -> PgTx ()
saveDocumentation (State.Documentation docs) =
  do
    beamTx $
      runDelete $ delete docTable (\_ -> val_ True)
    let rows = [ DocRow (T.pack $ display (pkgName pkgid))
                        (T.pack $ display (pkgVersion pkgid))
                        (T.pack $ blobMd5 blob)
               | (pkgid, blob) <- Map.toList docs ]
    mapM_ insertDocChunk (chunksOf 1000 rows)

insertDocChunk :: [DocRowT Identity] -> PgTx ()
insertDocChunk chunk =
  beamTx $
    runInsert $ insert docTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

documentationStateComponent :: String -> PgConnection -> IO (StateComponent AcidState State.Documentation)
documentationStateComponent name serverPgConn = do
  -- Load state
  loaded <- runPgTx serverPgConn loadDocumentation

  pgSt <- mkAcidState serverPgConn loaded saveDocumentation
  return StateComponent {
      stateDesc    = "Package documentation"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent State.GetDocumentation)
    , putState     = \s -> do
        runPgTx serverPgConn (saveDocumentation s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

