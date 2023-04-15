module ProgressBar where

import Control.Monad (forM)
import System.IO (hFlush, stdout)

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
