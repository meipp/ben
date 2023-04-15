module Classification where

import System.Process
import System.Exit
import System.IO (hPutStr, hClose)
import Control.Monad (forM, join)

classifyOne :: CreateProcess -> String -> IO Bool
classifyOne cmd input = do
    (Just stdin, Just stdout, _, p) <- createProcess cmd { std_in = CreatePipe, std_out = CreatePipe }
    hPutStr stdin input
    hClose stdin
    result <- waitForProcess p
    hClose stdout
    return $ result == ExitSuccess

classifications' :: [(String, String)] -> String -> IO [String]
classifications' classifiers input = fmap join $ forM classifiers $ \(name, cmd) -> do
    result <- classifyOne (shell cmd) input
    return $ if result then [name] else []

classify :: [(String, String)] -> String -> IO [String]
classify classifiers stdout = do
    classes <- classifications' classifiers stdout
    return $ classes
