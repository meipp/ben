module CmdLine (
    CmdLineArgs(..),
    parseArgs,
) where

import Control.Monad (when)
import Data.Fixed (Fixed, E6)
import Options.Applicative

data CmdLineArgs = CmdLineArgs {
    programs :: [(String, String)],
    files :: FilePath,
    classifiers :: [(String, String)],
    jobs :: Int,
    timeoutMicroseconds :: Int
} deriving (Show)

parser :: Parser CmdLineArgs
parser = CmdLineArgs
    <$> parsePrograms
    <*> parseSource
    <*> parseClassifiers
    <*> parseJobs
    <*> parseTimeout

parseLabeledCommand :: String -> (String, String)
parseLabeledCommand s =
    -- TODO
    case break (== ':') s of
      (cmd, "") -> (cmd, cmd)
      (name, (':' : cmd)) -> (name, dropWhile (== ' ') cmd)

positiveNumber :: (Read a, Num a, Ord a) => ReadM a
positiveNumber = do
    x <- auto
    when (x <= 0) (fail "must be positive")
    return x

parseMicroseconds :: ReadM Int
parseMicroseconds = do
    t <- positiveNumber :: ReadM (Fixed E6)
    return (round (t * 1000000))

parsePrograms :: Parser [(String, String)]
parsePrograms = some (parseLabeledCommand <$> strOption (
        long "program" <> short 'p' <> metavar "CMD" <> help "Program to benchmark"
    ))

parseSource :: Parser FilePath
parseSource = strOption (
        long "source" <> short 's' <> metavar "PATH" <> help "Location of the test instances"
    )

parseClassifiers :: Parser [(String, String)]
parseClassifiers = many (parseLabeledCommand <$> (strOption (
        long "classifier" <> short 'c' <> metavar "CMD" <> help "Classifier command")
    ))

parseJobs :: Parser Int
parseJobs = option positiveNumber (
        long "jobs" <> short 'j' <> metavar "N" <> help "Number of processes to run in parallel" <> value 1
    )

parseTimeout :: Parser Int
parseTimeout = option parseMicroseconds (
        long "timeout" <> short 't' <> metavar "SECONDS" <> help "Timeout in seconds (default: 60)" <> value 60000000
    )

parseArgs :: IO CmdLineArgs
parseArgs = execParser opts
    where
    opts = info (parser <**> helper)
        ( fullDesc
        <> progDesc "Benchmark a set of programs on a set of files"
        <> header "ben - run benchmarks"
        )
