{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Measurement where

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

seconds :: Int
seconds = 1000000

milliseconds :: NominalDiffTime -> Int
milliseconds = round . (* 1000)

measureCommand :: (String, String) -> FilePath -> IO Measurement
-- TODO filenames containing spaces etc
measureCommand (name, cmd) file = do
    start <- getCurrentTime
    (_, Just stdout, Just stderr, p) <- createProcess (shell (cmd ++ " " ++ file)) { std_out = CreatePipe, std_err = CreatePipe }
    result <- race (threadDelay (2*seconds)) (waitForProcess p)
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start)

    when (result == Left ()) (terminateProcess p)

    Measurement name (cmd ++ " " ++ file) <$> pure (
        case result of
            Left ()             -> Timeout
            Right ExitSuccess     -> Success
            Right (ExitFailure n) -> Failure n) <*> hGetContents' stdout <*> hGetContents' stderr <*> pure (milliseconds diff) <*> pure []
