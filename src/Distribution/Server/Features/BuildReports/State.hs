{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Features.BuildReports.State where

import Distribution.Server.Features.BuildReports.BuildReports
                (BuildReports)
import qualified Distribution.Server.Features.BuildReports.BuildReports as BuildReports

initialBuildReports :: BuildReports
initialBuildReports = BuildReports.emptyReports
