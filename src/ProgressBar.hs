module ProgressBar (
    parallelizeWithProgressBar,
) where

import Control.Monad (when)
import System.IO (hFlush, stdout)
import Prelude hiding (init)
import UnliftIO.Async

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
        render = renderBar prefix suffix n w
        display i = displayLine redraw (render i)

renderBar :: String -> String -> Int -> Int -> Int -> String
renderBar prefix suffix n w i = prefix +++ bar +++ suffix
    where
        bar
            | n == 0    = "[" ++ replicate w '0' ++ "] " ++ show i ++ "/" ++ show n
            | n == i    = "[" ++ replicate w '=' ++ "] " ++ show i ++ "/" ++ show n
            | otherwise = "[" ++ replicate u '=' ++ ">" ++ replicate (w - u - 1) ' ' ++ "] " ++ show i ++ "/" ++ show n

        u = w * i `div` n

        "" +++ x = x
        x +++ "" = x
        x +++  y = x ++ " " ++ y

parallelizeWithProgressBar :: Int -> (a -> IO b) -> [a] -> IO [b]
parallelizeWithProgressBar j f xs = do
    let n = length xs
    let progress = progressBar True True "" "" n 40
    init progress
    ys <- pooledMapConcurrentlyN j (\(i, x) -> progress `display` i >> f x) (zip [0..] xs)
    close progress
    return ys
