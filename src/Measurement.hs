{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Measurement (
    Measurement(..),
    Status(..),
    measureCommand,
) where

import CmdLine
import Classification (classify)

import System.Process
import System.Exit (ExitCode(..))
import System.IO (hGetContents', hClose)
import Data.Time
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import GHC.Generics (Generic)
import Data.Aeson (toJSON, ToJSON)

-- | Defines the exit status of a (possibly timed out) process.
data Status
    = Success     -- ^ indicates successful termination
    | Failure Int -- ^ indicates program failure with exit code
    | Timeout     -- ^ indicates that process the process did not terminate on time and had to be killed
    deriving (Eq, Ord, Show)

instance ToJSON Status where
    toJSON Success = toJSON "0"
    toJSON (Failure n) = toJSON (show n)
    toJSON Timeout = toJSON "timeout"

-- | The result of running and timing a command.
--
-- This includes all results immediately produced by the command ('status', 'stdout', 'stderr'),
-- all metadata such as 'program' and 'command',
-- and all further processing like the running 'time' and the 'classifications' of the produced 'stdout'.
data Measurement = Measurement {
    program :: String,          -- ^ the program name or assigned label
    command :: String,          -- ^ the actual command as invoked in the shell
    status :: Status,           -- ^ exit status
    stdout :: String,
    stderr :: String,
    time :: Int,                -- ^ running time in milliseconds
    classifications :: [String] -- ^ all classifiers that returned 'True' on 'stdout'
} deriving (Show, Generic, ToJSON)

milliseconds :: NominalDiffTime -> Int
milliseconds = round . (* 1000)

-- | Measures how long an 'IO' action takes to complete.
--
-- Performs an 'IO' action and takes start and end time.
-- The measured time is of type 'NominalDiffTime' and has a precision of one picosecond @(= 10^-12 s)@.
timeIO :: IO a -> IO (NominalDiffTime, a)
timeIO action = do
    start <- getCurrentTime
    x <- action
    end <- getCurrentTime
    return (diffUTCTime end start, x)

-- | Runs a command and kills it on timeout.
--
-- Takes a timeout (in microseconds) and a command and runs it, capturing its exit status, stdout and stderr.
-- If the command times out, its process will be terminated and this function returns   @(@'Timeout'@, "", "")@.
runCommandWithTimeout :: Int -> String -> IO (Status, String, String)
runCommandWithTimeout timeoutMicroseconds cmd = do
    (_, Just stdout, Just stderr, p) <- createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
    result <- race (threadDelay timeoutMicroseconds) (waitForProcess p)

    let status = case result of
            Left () -> Timeout
            Right ExitSuccess -> Success
            Right (ExitFailure n) -> Failure n

    case status of
        Timeout -> do
            terminateProcess p
            hClose stdout
            hClose stderr
            -- cannot read 'stdout' and 'stderr' on 'Timeout', because 'hGetContents' hangs up
            return (status, "", "")
        _ -> do
            stdout' <- hGetContents' stdout
            stderr' <- hGetContents' stderr
            return (status, stdout', stderr')

-- | Runs, times and potentially terminates a command on an input file, producing a 'Measurement'.
--
-- This function will:
--
--  * run a command on an input file
--
--  * terminate it on timeout
--
--  * measure its running time
--
--  * capture exit status, stdout, stderr
--
--  * run classifiers on its stdout
--
--  * ultimately return a 'Measurement'
measureCommand :: CmdLineArgs -> Labeled String -> FilePath -> IO Measurement
-- TODO filenames containing spaces etc
measureCommand CmdLineArgs{classifiers, timeoutMicroseconds} (name, cmd) file = do
    let command = cmd ++ " " ++ file
    (diff, (status, stdout, stderr)) <- timeIO $ runCommandWithTimeout timeoutMicroseconds command
    classifications <- classify classifiers stdout

    return $ Measurement{
            program=name,
            command,
            status,
            stdout,
            stderr,
            time=milliseconds diff,
            classifications
        }
