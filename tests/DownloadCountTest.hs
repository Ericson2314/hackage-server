module Main where

import Control.Monad (forM_, unless)
import Control.Monad.State (State, execState)
import qualified Data.Map.Strict as Map
import Data.Time.Calendar (fromGregorian)
import Distribution.Package (PackageIdentifier (..), mkPackageName)
import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Util.CountingMap (cmEmpty, cmFind, cmInsert, cmToList, cmTotal)
import Distribution.Version (mkVersion)
import System.Exit (die)

main :: IO ()
main = do
  -- Recording downloads one at a time and recording them as a batch must
  -- produce exactly the same statistics.
  oneAtATime <- withStats $
    forM_ downloadStream registerDownload
  batched <- withStats $
    registerDownloads (tally downloadStream)
  unless (oneAtATime == batched) $
    die $
      "Batched downloads do not match individual downloads:\n"
        ++ show (cmToList (inMemCounts oneAtATime))
        ++ "\nversus\n"
        ++ show (cmToList (inMemCounts batched))

  -- Sanity check the counts themselves, so that the test above cannot be
  -- satisfied by both paths being equally wrong.
  let counts = inMemCounts batched
  unless (cmTotal counts == length downloadStream) $
    die $
      "Wrong total: "
        ++ show (cmTotal counts)
        ++ " expected "
        ++ show (length downloadStream)
  forM_ (tally downloadStream) $ \(pkgid, n) ->
    unless (cmFind pkgid counts == n) $
      die $
        "Wrong count for "
          ++ show pkgid
          ++ ": "
          ++ show (cmFind pkgid counts)
          ++ " expected "
          ++ show n

  -- Splitting a batch across several updates must accumulate, not replace.
  split <- withStats $ do
    registerDownloads (tally (take 3 downloadStream))
    registerDownloads (tally (drop 3 downloadStream))
  unless (split == batched) $
    die "Successive batches do not accumulate"

  -- An empty batch must be a no-op, and in particular must not disturb the
  -- day the statistics are recorded against.
  empty <- withStats $ registerDownloads []
  unless (inMemCounts empty == inMemCounts (initInMemStats today)) $
    die "Empty batch changed the counts"
  unless (inMemToday batched == today) $
    die "Recording downloads changed the recorded day"

  putStrLn "OK"
  where
    withStats :: State InMemStats () -> IO InMemStats
    withStats action = return $ execState action (initInMemStats today)

    today = fromGregorian 2026 8 24

    -- Deliberately interleaved and repeated, so that ordering differences
    -- between the two paths would show up.
    downloadStream =
      [ pkg "foo" [1, 0],
        pkg "bar" [2],
        pkg "foo" [1, 0],
        pkg "foo" [2, 1],
        pkg "bar" [2],
        pkg "foo" [1, 0]
      ]

    pkg name version =
      PackageIdentifier (mkPackageName name) (mkVersion version)

tally :: [PackageIdentifier] -> [(PackageIdentifier, Int)]
tally = Map.toList . Map.fromListWith (+) . map (,1)

