{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Measurement where

import CmdLine
import Classification

import System.Process
import System.Exit
import System.IO (hGetContents')
import Data.Time
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (when)
import GHC.Generics (Generic)
import Data.Aeson (toJSON, ToJSON)

data Status = Success | Failure Int | Timeout deriving (Eq, Ord, Show)

instance ToJSON Status where
    toJSON Success = toJSON "0"
    toJSON (Failure n) = toJSON (show n)
    toJSON Timeout = toJSON "timeout"

data Measurement = Measurement {
    program :: String,
    command :: String,
    status :: Status,
    stdout :: String,
    stderr :: String,
    time :: Int,
    classifications :: [String]
} deriving (Show, Generic, ToJSON)

milliseconds :: NominalDiffTime -> Int
milliseconds = round . (* 1000)

timeIO :: IO a -> IO (NominalDiffTime, a)
timeIO action = do
    start <- getCurrentTime
    x <- action
    end <- getCurrentTime
    let diff = diffUTCTime end start
    return (diff, x)

runCommandWithTimeout :: Int -> String -> IO (Status, String, String)
runCommandWithTimeout timeoutMicroseconds cmd = do
    (_, Just stdout, Just stderr, p) <- createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
    result <- race (threadDelay timeoutMicroseconds) (waitForProcess p)
    when (result == Left ()) (terminateProcess p)
    stdout' <- hGetContents' stdout
    stderr' <- hGetContents' stderr

    let status = case result of
            Left () -> Timeout
            Right ExitSuccess -> Success
            Right (ExitFailure n) -> Failure n

    return (status, stdout', stdout')

measureCommand :: CmdLineArgs -> (String, String) -> FilePath -> IO Measurement
-- TODO filenames containing spaces etc
measureCommand args (name, cmd) file = do
    let command = cmd ++ " " ++ file
    (diff, (status, stdout, stderr)) <- timeIO $ runCommandWithTimeout (timeoutMicroseconds args) command
    classifications <- classify (classifiers args) stdout

    return $ Measurement{
            program=name,
            command,
            status,
            stdout,
            stderr,
            time=milliseconds diff,
            classifications
        }
