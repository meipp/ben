{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Measurement
import Classification
import Analysis
import Tables
import CmdLine
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL (putStr)
import System.Directory.PathWalk

benchmark :: CmdLineArgs -> [(String, String)] -> FilePath -> [(String, String)] -> IO ()
benchmark args commands files classifiers = do
    fs <- find files
    measurements <- mapM (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    ms <- mapM (classify classifiers) measurements
    -- BL.putStr (encode ms)
    analyze (map fst classifiers) ms

-- recursively enumerates directory
find :: FilePath -> IO [FilePath]
find dir = do
    fs <- pathWalkLazy dir
    return $ fs >>= \(d, _, fs) -> map ((d ++ "/") ++) fs

main :: IO ()
main = parseArgs >>= run

run :: CmdLineArgs -> IO ()
run args@CmdLineArgs{programs, files, classifiers, timeoutMicroseconds} = do
    benchmark args programs files classifiers
