{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Cassava 'ToField'/'FromField' instances for Cabal/hackage types.
module Distribution.Server.Framework.CsvInstances () where

import Data.Csv (ToField(..), FromField(..), Parser)
import qualified Data.ByteString.Char8 as BS8
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Package (PackageName, PackageIdentifier)
import Distribution.Version (Version)
import Distribution.System (OS, Arch)
import Distribution.Compiler (CompilerId)
import Distribution.Server.Users.Types (UserId(..))
import Distribution.Server.Features.Tags.Types (Tag)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Features.BuildReports.BuildReport (BuildCovg, parseCovg)

prettyField :: (a -> String) -> a -> BS8.ByteString
prettyField f = BS8.pack . f

parsecParseField :: (String -> Maybe a) -> BS8.ByteString -> Parser a
parsecParseField f bs = case f (BS8.unpack bs) of
  Just a  -> pure a
  Nothing -> fail $ "parse: " ++ BS8.unpack bs

instance ToField UserId where toField (UserId n) = BS8.pack (show n)
instance FromField UserId where
  parseField bs = case reads (BS8.unpack bs) of { [(n, "")] -> pure (UserId n); _ -> fail $ "UserId: " ++ BS8.unpack bs }

instance ToField PackageName       where toField = prettyField prettyShow
instance ToField PackageIdentifier where toField = prettyField prettyShow
instance ToField Version           where toField = prettyField prettyShow
instance ToField OS                where toField = prettyField prettyShow
instance ToField Arch              where toField = prettyField prettyShow
instance ToField CompilerId        where toField = prettyField prettyShow
instance ToField Tag               where toField = prettyField prettyShow

instance FromField PackageName       where parseField = parsecParseField simpleParsec
instance FromField PackageIdentifier where parseField = parsecParseField simpleParsec
instance FromField Version           where parseField = parsecParseField simpleParsec
instance FromField OS                where parseField = parsecParseField simpleParsec
instance FromField Arch              where parseField = parsecParseField simpleParsec
instance FromField CompilerId        where parseField = parsecParseField simpleParsec
instance FromField Tag               where parseField = parsecParseField simpleParsec

instance ToField BlobStorage.BlobId where toField bid = BS8.pack ("\\x" ++ BlobStorage.blobMd5 bid)
instance FromField BlobStorage.BlobId where
  parseField bs = case BlobStorage.readBlobId (stripByteaHexBS bs) of
    Right bid -> pure bid
    Left err  -> fail err
    where
      stripByteaHexBS s = BS8.unpack $ case BS8.stripPrefix (BS8.pack "\\x") s of
        Just rest -> rest
        Nothing   -> s

instance ToField BuildCovg where toField = BS8.pack . show
instance FromField BuildCovg where parseField = pure . parseCovg . BS8.unpack
