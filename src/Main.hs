{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Measurement (measureCommand)
import Classification
import Analysis
import Tables
import CmdLine
import Control.Monad (forM)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL (putStr)
import System.Directory.PathWalk
import System.IO

putStrFlush :: String -> IO ()
putStrFlush s = putStr s >> hFlush stdout

progressBar :: Int -> Int -> Int -> String
progressBar n w i
    | n == 0    = "[" ++ replicate w '0' ++ "] " ++ show i ++ "/" ++ show n
    | n == i    = "[" ++ replicate w '=' ++ "] " ++ show i ++ "/" ++ show n
    | otherwise = "[" ++ replicate u '=' ++ ">" ++ replicate (w - u - 1) ' ' ++ "] " ++ show i ++ "/" ++ show n
    where u = if w < n
                then i `div` (n `div` w)
                else i * (w `div` n)

forMProgress :: [a] -> (a -> IO b) -> IO ([b])
forMProgress xs f = do
    let n = length xs
    let progress = progressBar n 40
    ys <- forM (zip [0..] xs) $ \(i, x) -> putStrFlush ("\rProgress: " ++ progress i) >> f x
    putStrFlush ("\rProgress: " ++ progress n)
    putStr "\n"
    return ys

benchmark :: CmdLineArgs -> [(String, String)] -> FilePath -> [(String, String)] -> IO ()
benchmark args commands files classifiers = do
    fs <- find files
    -- measurements <- mapM (uncurry (measureCommand args)) [(c, f) | c <- commands, f <- fs]
    measurements <- forMProgress [(c, f) | c <- commands, f <- fs] (uncurry (measureCommand args))
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
