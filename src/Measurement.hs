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

measureCommand :: CmdLineArgs -> (String, String) -> FilePath -> IO Measurement
-- TODO filenames containing spaces etc
measureCommand args (name, cmd) file = do
    start <- getCurrentTime
    (_, Just stdout, Just stderr, p) <- createProcess (shell (cmd ++ " " ++ file)) { std_out = CreatePipe, std_err = CreatePipe }
    result <- race (threadDelay (timeoutMicroseconds args)) (waitForProcess p)
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start)

    when (result == Left ()) (terminateProcess p)

    stdout' <- hGetContents' stdout
    Measurement name (cmd ++ " " ++ file) <$> pure (
        case result of
            Left ()             -> Timeout
            Right ExitSuccess     -> Success
            Right (ExitFailure n) -> Failure n) <*> pure stdout' <*> hGetContents' stderr <*> pure (milliseconds diff) <*> classify (classifiers args) stdout'
