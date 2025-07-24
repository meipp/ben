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

benchmark :: CmdLineArgs -> IO ()
benchmark args@CmdLineArgs{programs, files, classifiers, jobs, jsonExport, repetitions} = do
    let commands = concat $ replicate repetitions programs
    fs <- find' files
    -- measurements <- mapM (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    measurements <- parallelizeWithProgressBar jobs (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    analyze (map fst classifiers) measurements

    when jsonExport $ printJSON measurements

printJSON :: ToJSON a => a -> IO ()
printJSON x = do
    BL.putStr (encode x)
    putStrLn ""

-- recursively enumerates directory
find :: FilePath -> IO [FilePath]
find path = do
    isFile <- doesFileExist path
    if isFile then
        return [path]
    else do
            fs <- pathWalkLazy path
            return $ concatMap (\(d, _, fs') -> (</>) <$> [d] <*> fs') fs

find' :: [FilePath] -> IO [FilePath]
find' paths = concat <$> mapM find paths

main :: IO ()
main = parseArgs >>= benchmark
