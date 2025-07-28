module Main (main) where

import Measurement (measureCommand)
import Analysis (analyze)
import CmdLine
import ProgressBar (parallelizeWithProgressBar)
import Data.Aeson (encode, ToJSON)
import qualified Data.ByteString.Lazy as BL (putStr)
import System.Directory.PathWalk (pathWalkLazy)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Monad (when)

-- | Perform a benchmark as described in 'CmdLineArgs'.
--
-- This runs all 'programs' on all 'files'.
-- If 'jobs' > 1, this many calls will be run in parallel.
-- If 'repetitions' > 1, the benchmark is re-run this many times.
-- If 'jsonExport' is set, a JSON dump of all measurements will be printed.
benchmark :: CmdLineArgs -> IO ()
benchmark args@CmdLineArgs{programs, files, classifiers, jobs, jsonExport, repetitions} = do
    let commands = concat $ replicate repetitions programs
    fs <- find' files
    -- measurements <- mapM (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    measurements <- parallelizeWithProgressBar jobs (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    analyze (map fst classifiers) measurements

    when jsonExport $ do
        putStrLn ""
        printJSON measurements

printJSON :: ToJSON a => a -> IO ()
printJSON x = do
    BL.putStr (encode x)
    putStrLn ""

-- | Recursively enumerates all files at a given directory subtree.
--
-- If @path@ is a regular file, only @[path]@ is returned.
-- If @path@ is a directory, all regular files in the subtree of @path@ will be returned.
-- All paths are relative to @path@ and not normalized (unless @path@ is already normalized).
-- Symlinks are not handled.
find :: FilePath -> IO [FilePath]
find path = do
    isFile <- doesFileExist path
    if isFile then
        return [path]
    else do
            fs <- pathWalkLazy path
            return $ concatMap (\(d, _, fs') -> (</>) <$> [d] <*> fs') fs

-- | Recursively enumerates all files at given directory subtrees.
--
-- Like 'find' but takes a list of paths.
find' :: [FilePath] -> IO [FilePath]
find' paths = concat <$> mapM find paths

-- | Entrypoint of the program. Reads command line arguments and runs 'benchmark'.
main :: IO ()
main = parseArgs >>= benchmark
