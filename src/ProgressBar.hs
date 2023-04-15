{-# LANGUAGE NamedFieldPuns #-}

module ProgressBar where

import Control.Monad (forM, when)
import System.IO (hFlush, stdout)
import Prelude hiding (init)

displayLine :: Bool -> String -> IO ()
displayLine True  s = putStr ("\r" ++ s) >> hFlush stdout
displayLine False s = putStrLn s

data ProgressBar = ProgressBar { init :: IO (), display :: Int -> IO (), close :: IO () }

progressBar :: Bool -> Bool -> String -> String -> Int -> Int -> ProgressBar
progressBar redraw hideOnClose prefix suffix n w = ProgressBar{
        init = display 0,
        display,
        close = do
            display n
            when (redraw && hideOnClose) (putStr ("\r" ++ replicate (length (render n)) ' ' ++ "\r"))
            when (redraw && not hideOnClose) (putStr "\n")
    }
    where
        render i = renderBar prefix suffix n w i
        display i = displayLine redraw (render i)

renderBar :: String -> String -> Int -> Int -> Int -> String
renderBar prefix suffix n w i = (prefix +++ bar +++ suffix)
    where
        bar
            | n == 0    = "[" ++ replicate w '0' ++ "] " ++ show i ++ "/" ++ show n
            | n == i    = "[" ++ replicate w '=' ++ "] " ++ show i ++ "/" ++ show n
            | otherwise = "[" ++ replicate u '=' ++ ">" ++ replicate (w - u - 1) ' ' ++ "] " ++ show i ++ "/" ++ show n

        u = w * i `div` n

        "" +++ x = x
        x +++ "" = x
        x +++  y = x ++ " " ++ y

withProgress :: ProgressBar -> [a] -> (a -> IO b) -> IO ([b])
withProgress progress xs f = do
    init progress
    ys <- forM (zip [0..] xs) $ \(i, x) -> progress `display` i >> f x
    close progress
    return ys

forMProgress :: [a] -> (a -> IO b) -> IO ([b])
forMProgress xs f = do
    let n = length xs
    let progress = progressBar True True "" "" n 40
    withProgress progress xs f
