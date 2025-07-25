module Classification (
    classify,
) where

import System.Process
import System.Exit (ExitCode(..))
import System.IO (hPutStr, hClose)
import Control.Monad (filterM)
import CmdLine (Labeled)

runCommandSuppressingOutput :: CreateProcess -> String -> IO ExitCode
runCommandSuppressingOutput cmd input = do
    (Just stdin, Just stdout, Just stderr, p) <- createProcess cmd { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    hPutStr stdin input
    hClose stdin
    exitCode <- waitForProcess p
    hClose stdout
    hClose stderr
    return $ exitCode

classify :: [Labeled String] -> String -> IO [String]
classify classifiers input = map fst <$> filterM classifierMatchesInput classifiers
    where
        classifierMatchesInput :: Labeled String -> IO Bool
        classifierMatchesInput (_, cmd) =
            (== ExitSuccess) <$> runCommandSuppressingOutput (shell cmd) input
