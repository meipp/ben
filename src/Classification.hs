module Classification (
    classify,
) where

import System.Process
import System.Exit (ExitCode(..))
import System.IO (hPutStr, hClose)
import Control.Monad (filterM)
import CmdLine (Labeled)

-- | Runs a command, ignoring its output.
--
-- Takes a command and an input 'String' and runs it, passing the 'String' argument as @stdin@.
--
-- 'stdout' and 'stderr' are ignored, the command's 'ExitCode' is captured and returned.
runCommandSuppressingOutput :: CreateProcess -> String -> IO ExitCode
runCommandSuppressingOutput cmd input = do
    (Just stdin, Just stdout, Just stderr, p) <- createProcess cmd { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    hPutStr stdin input
    hClose stdin
    exitCode <- waitForProcess p
    hClose stdout
    hClose stderr
    return $ exitCode

-- | Classifies an input 'String' according to a set of classifiers.
--
-- Takes a list of 'Labeled' classifier commands and an input 'String' and matches it against the classifiers.
-- A classifier command matches, if it produces 'ExitSuccess' on the supplied input.
-- Returns a list of the labels of all matching classifiers.
classify :: [Labeled String] -> String -> IO [String]
classify classifiers input = map fst <$> filterM classifierMatchesInput classifiers
    where
        classifierMatchesInput :: Labeled String -> IO Bool
        classifierMatchesInput (_, cmd) =
            (== ExitSuccess) <$> runCommandSuppressingOutput (shell cmd) input
