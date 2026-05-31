{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.Documentation.State where

import Distribution.Package
import Distribution.Server.Framework.BlobStorage (BlobId)
import Data.TarIndex () -- For SafeCopy instances
import Distribution.Server.Framework.MemSize

import Data.SafeCopy (base, deriveSafeCopy)

import qualified Data.Map as Map

---------------------------------- Documentation
data Documentation = Documentation {
     documentation :: !(Map.Map PackageIdentifier BlobId)
   } deriving (Show, Eq)

deriveSafeCopy 0 'base ''Documentation

instance MemSize Documentation where
    memSize (Documentation a) = memSize1 a

initialDocumentation :: Documentation
initialDocumentation = Documentation Map.empty

