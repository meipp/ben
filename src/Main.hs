module Main (main) where

import Measurement (measureCommand)
import Analysis
import CmdLine
import ProgressBar
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL (putStr)
import System.Directory.PathWalk
import Control.Monad (when)

benchmark :: CmdLineArgs -> IO ()
benchmark args@CmdLineArgs{programs=commands, files, classifiers, jobs, jsonExport} = do
    fs <- find files
    -- measurements <- mapM (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    measurements <- parallelizeWithProgressBar jobs (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    analyze (map fst classifiers) measurements

    when jsonExport $ do
        BL.putStr (encode measurements)
        putStrLn ""

-- recursively enumerates directory
find :: FilePath -> IO [FilePath]
find dir = do
    fs <- pathWalkLazy dir
    return $ fs >>= \(d, _, fs') -> map ((d ++ "/") ++) fs'

main :: IO ()
main = parseArgs >>= benchmark
