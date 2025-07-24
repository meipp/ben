module CmdLine (
    CmdLineArgs(..),
    Labeled,
    parseArgs,
) where

import Control.Monad (when)
import Data.Fixed (Fixed, E6)
import Options.Applicative

data CmdLineArgs = CmdLineArgs {
    programs :: [Labeled String],
    files :: FilePath,
    classifiers :: [Labeled String],
    jobs :: Int,
    timeoutMicroseconds :: Int,
    jsonExport :: Bool
} deriving (Show)

-- Type alias for an a value with an assigned name
type Labeled a = (String, a)

parser :: Parser CmdLineArgs
parser = CmdLineArgs
    <$> parsePrograms
    <*> parseSource
    <*> parseClassifiers
    <*> parseJobs
    <*> parseTimeout
    <*> parseJSONExport

parseLabeledCommand :: String -> Labeled String
parseLabeledCommand s =
    case break (== ':') s of
      (cmd, "") -> (cmd, cmd)
      (name, ':' : cmd) -> (name, dropWhile (== ' ') cmd)
      -- Unreachable
      _ -> undefined

positiveNumber :: (Read a, Num a, Ord a) => ReadM a
positiveNumber = do
    x <- auto
    when (x <= 0) (fail "must be positive")
    return x

parseMicroseconds :: ReadM Int
parseMicroseconds = do
    t <- positiveNumber :: ReadM (Fixed E6)
    return (round (t * 1000000))

parsePrograms :: Parser [Labeled String]
parsePrograms = some (parseLabeledCommand <$> strOption (
        long "program" <> short 'p' <> metavar "CMD" <> help "Program to benchmark"
    ))

parseSource :: Parser FilePath
parseSource = strOption (
        long "source" <> short 's' <> metavar "PATH" <> help "Location of the test instances"
    )

parseClassifiers :: Parser [Labeled String]
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

parseJSONExport :: Parser Bool
parseJSONExport = switch (
        long "json" <> short 'J' <> help "Show report in JSON format"
    )

parseArgs :: IO CmdLineArgs
parseArgs = execParser opts
    where
    opts = info (parser <**> helper)
        ( fullDesc
        <> progDesc "Benchmark a set of programs on a set of files"
        <> header "ben - run benchmarks"
        )
