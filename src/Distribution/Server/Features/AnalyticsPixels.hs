{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, OverloadedStrings #-}

-- | Implements a system to allow users to attach analytics pixels to packages.
--
module Distribution.Server.Features.AnalyticsPixels
  ( AnalyticsPixelsFeature(..)
  , AnalyticsPixel(..)
  , initAnalyticsPixelsFeature
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Aeson (toJSON, Value(..), object, (.=))

import Distribution.Server.Features.AnalyticsPixels.Acid (acidStore)
import qualified Distribution.Server.Features.AnalyticsPixels.Store as Store
import Distribution.Server.Features.AnalyticsPixels.Types

import Distribution.Server.Framework

import Distribution.Package (PackageName)
import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users

-- | Define the prototype for this feature
data AnalyticsPixelsFeature = AnalyticsPixelsFeature {
    analyticsPixelsFeatureInterface :: HackageFeature,
    analyticsPixelsResource         :: Resource,
    userAnalyticsPixelsResource     :: Resource,

    analyticsPixelAdded             :: Hook (PackageName, AnalyticsPixel) (),
    analyticsPixelRemoved           :: Hook (PackageName, AnalyticsPixel) (),

    -- | Returns all 'AnalyticsPixel's associated with a 'Package'.
    getPackageAnalyticsPixels       :: forall m. MonadIO m => PackageName -> m (Set AnalyticsPixel),

    -- | Adds a new 'AnalyticsPixel' to a 'Package'. Returns True in case it was added. False in case
    -- it's already existing.
    addPackageAnalyticsPixel        :: forall m. MonadIO m => PackageName -> AnalyticsPixel -> m Bool,

    -- | Remove a 'AnalyticsPixel' from a 'Package'.
    removePackageAnalyticsPixel     :: forall m. MonadIO m => PackageName -> AnalyticsPixel -> m ()
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature AnalyticsPixelsFeature where
  getFeatureInterface = analyticsPixelsFeatureInterface

-- | Called from Features.hs to initialize this feature
initAnalyticsPixelsFeature :: ServerEnv
                          -> IO ( CoreFeature
                            -> UserFeature
                            -> UploadFeature
                            -> IO AnalyticsPixelsFeature)
initAnalyticsPixelsFeature env@ServerEnv{serverStateDir} = do
  dbAnalyticsPixelsState <- acidStore serverStateDir
  analyticsPixelAdded    <- newHook
  analyticsPixelRemoved  <- newHook

  return $ \coref@CoreFeature{..} userf@UserFeature{..} uploadf -> do
    let feature = analyticsPixelsFeature env
                  dbAnalyticsPixelsState
                  coref userf uploadf analyticsPixelAdded analyticsPixelRemoved

    return feature

-- | Default constructor for building this feature.
analyticsPixelsFeature :: ServerEnv
                      -> Store.Backend
                      -> CoreFeature                          -- To get site package list
                      -> UserFeature                          -- To authenticate users
                      -> UploadFeature                        -- For accessing package maintainers and trustees
                      -> Hook (PackageName, AnalyticsPixel) () -- Signals addition of a new AnalyticsPixel
                      -> Hook (PackageName, AnalyticsPixel) () -- Signals removeal of a AnalyticsPixel
                      -> AnalyticsPixelsFeature

analyticsPixelsFeature  ServerEnv{..}
              Store.Backend{backendStore = analyticsPixelsState, backendState}
              CoreFeature { coreResource = CoreResource{..} }
              UserFeature{..}
              UploadFeature{..}
              analyticsPixelAdded
              analyticsPixelRemoved
  = AnalyticsPixelsFeature {..}
  where
    analyticsPixelsFeatureInterface  = (emptyHackageFeature "AnalyticsPixels") {
        featureDesc      = "Allow users to attach analytics pixels to their packages",
        featureResources = [analyticsPixelsResource, userAnalyticsPixelsResource]
      , featureState     = backendState
      }

    analyticsPixelsResource :: Resource
    analyticsPixelsResource = (resourceAt "/package/:package/analytics-pixels.:format") {
        resourceGet  = [("json", serveGetPixelsJSON)]
      , resourcePost = [("json", serveAddPixelJSON)]
      }

    userAnalyticsPixelsResource :: Resource
    userAnalyticsPixelsResource = resourceAt "/user/:username/analytics-pixels.:format"

    serveGetPixelsJSON :: DynamicPath -> ServerPartE Response
    serveGetPixelsJSON dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      pixels <- getPackageAnalyticsPixels pkgname
      ok . toResponse $ toJSON
        [ analyticsPixelUrl p | p <- Set.toList pixels ]

    serveAddPixelJSON :: DynamicPath -> ServerPartE Response
    serveAddPixelJSON dpath = do
      _uid <- guardAuthorised [InGroup trusteesGroup]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      pixelUrl <- look "analytics-pixel"
      let pixel = AnalyticsPixel (T.pack pixelUrl)
      added <- addPackageAnalyticsPixel pkgname pixel
      ok . toResponse $ object ["added" .= added]

    getPackageAnalyticsPixels :: MonadIO m => PackageName -> m (Set AnalyticsPixel)
    getPackageAnalyticsPixels =
        Store.getPackageAnalyticsPixels analyticsPixelsState

    addPackageAnalyticsPixel :: MonadIO m => PackageName -> AnalyticsPixel -> m Bool
    addPackageAnalyticsPixel name pixel = do
        added <- Store.addPackageAnalyticsPixel analyticsPixelsState name pixel
        when added $ runHook_ analyticsPixelAdded (name, pixel)
        pure added

    removePackageAnalyticsPixel :: MonadIO m => PackageName -> AnalyticsPixel -> m ()
    removePackageAnalyticsPixel name pixel = do
        Store.removePackageAnalyticsPixel analyticsPixelsState name pixel
        runHook_ analyticsPixelRemoved (name, pixel)
