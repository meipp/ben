module CmdLine (
    CmdLineArgs(..),
    Labeled,
    parseArgs,
) where

import Control.Monad (when)
import Data.Fixed (Fixed, E6)
import Options.Applicative

-- | The result of running 'parseArgs'. A configuration of the benchmark to be run.
data CmdLineArgs = CmdLineArgs {
    programs :: [Labeled String],    -- ^ programs to benchmark
    files :: [FilePath],             -- ^ list of regular files or directories to search for files to pass to the programs
    classifiers :: [Labeled String], -- ^ classifier commands to be used by 'Classification.classify'
    jobs :: Int,                     -- ^ number of program calls to run in parallel
    timeoutMicroseconds :: Int,      -- ^ timeout per program call
    jsonExport :: Bool,              -- ^ whether to print a JSON dump of all measurements after completion
    repetitions :: Int               -- ^ number of times to re-run each program call
} deriving (Show)

-- | Type alias for an a value with an assigned name.
--
-- This allows a value to carry and retain a name and still be transformed through 'fmap'.
--
-- We'll use this for labeled commands of the form
-- @Program 1: ./program1 ARG1 ARG2@ where we want to run the command as @./program1 ARG1 ARG2@,
-- but display it as @Program 1@.
--
-- Both the @-p@ and @-c@ options support labeled commands.
type Labeled a = (String, a)

parser :: Parser CmdLineArgs
parser = CmdLineArgs
    <$> parsePrograms
    <*> parseSources
    <*> parseClassifiers
    <*> parseJobs
    <*> parseTimeout
    <*> parseJSONExport
    <*> parseRepetitions

-- | Split a labeled command into label and command part.
--
-- Labeled commands have the form
-- @Program 1: ./program1 ARG1 ARG2@ where we want to run the command as @./program1 ARG1 ARG2@,
-- but display it as @Program 1@.
--
-- This function takes an input of the form @X:Y@, splitting it on the first occurrence of @\':\'@,
-- dropping all trialing whitespaces from @Y@ and ultimately returns @(X, Y)@.
-- If the input does not contain @\':\'@, i.e. if there is no label supplied, the command itself is used as label:
-- Inputs of the form @X@ return @(X, X)@.
--
-- For example:
-- @"Timeout mentioned: grep ^timeout$"@ returns @("Timeout mentioned", "grep ^timeout$")@,
-- whereas @"grep ^timeout$"@ returns @("grep ^timeout$", "grep ^timeout$")@.
--
-- This means that commands that do contain a @\':\'@ __must__ be labeled, otherwise this function
-- will splice the command at the first occurrence of @\':\'@, confusing anything leading up to it for the label.
--
-- Both the @-p@ and @-c@ options support labeled commands.
parseLabeledCommand :: String -> Labeled String
parseLabeledCommand s =
    case break (== ':') s of
      (cmd, "") -> (cmd, cmd)
      (name, ':' : cmd) -> (name, dropWhile (== ' ') cmd)
      -- Unreachable
      _ -> undefined

-- | Helper to read arguments as positive numbers in a 'Parser'.
positiveNumber :: (Read a, Num a, Ord a) => ReadM a
positiveNumber = do
    x <- auto
    when (x <= 0) (fail "must be positive")
    return x

-- | Helper to read microseconds in a 'Parser'.
--
-- The input is written as seconds.
-- 1 denotes 1 second, i.e. 1000000 microseconds.
-- 0.5 denotes half a second.
-- 0.000001 denotes one microsecond.
-- The precision cutoff is at microseconds, so 0.0000009 is interpreted as 0.
parseMicroseconds :: ReadM Int
parseMicroseconds = do
    t <- positiveNumber :: ReadM (Fixed E6)
    return (round (t * 1000000))

parsePrograms :: Parser [Labeled String]
parsePrograms = some (parseLabeledCommand <$> strOption (
        long "program" <> short 'p' <> metavar "CMD" <> help "Program to benchmark"
    ))

parseSources :: Parser [FilePath]
parseSources = some (strOption (
        long "source" <> short 's' <> metavar "PATH" <> help "Location of the test instances"
    ))

parseClassifiers :: Parser [Labeled String]
parseClassifiers = many (parseLabeledCommand <$> strOption (
        long "classifier" <> short 'c' <> metavar "CMD" <> help "Classifier command")
    )

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

parseRepetitions :: Parser Int
parseRepetitions = option positiveNumber (
        long "repetitions" <> short 'r' <> metavar "N" <> help "Number of times to repeat running each program on each input" <> value 1
    )

-- | Runs the argument parser on the supplied command line arguments and returns a 'CmdLineArgs'.
parseArgs :: IO CmdLineArgs
parseArgs = execParser opts
    where
    opts = info (parser <**> helper)
        ( fullDesc
        <> progDesc "Benchmark a set of programs on a set of files"
        <> header "ben - run benchmarks"
        )
