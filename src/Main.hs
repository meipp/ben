module Main (main) where

import Measurement (measureCommand)
import Analysis
import CmdLine
import ProgressBar
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL (putStr)
import System.Directory.PathWalk

benchmark :: CmdLineArgs -> IO ()
benchmark args@CmdLineArgs{programs=commands, files, classifiers, jobs} = do
    fs <- find files
    -- measurements <- mapM (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    measurements <- parallelizeWithProgressBar jobs (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    -- BL.putStr (encode ms)
    analyze (map fst classifiers) measurements

-- recursively enumerates directory
find :: FilePath -> IO [FilePath]
find dir = do
    fs <- pathWalkLazy dir
    return $ fs >>= \(d, _, fs') -> map ((d ++ "/") ++) fs'

main :: IO ()
main = parseArgs >>= benchmark
