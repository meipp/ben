module CmdLine where

import Control.Monad (when)
import Data.Fixed (Fixed, E6)
import Options.Applicative

data CmdLineArgs = CmdLineArgs {
    programs :: [(String, String)],
    files :: FilePath,
    classifiers :: [(String, String)],
    timeoutMicroseconds :: Int
    } deriving (Show)

parser :: Parser CmdLineArgs
parser = CmdLineArgs
    <$> some (parseLabeledCommand <$> strOption
    (long "program"
    <> short 'p'
    <> metavar "CMD"
    <> help "Program to benchmark")
    )
    <*> ( strOption
    (long "source"
    <> short 's'
    <> metavar "PATH"
    <> help "Location of the test instances")
    )
    <*> many (fmap parseLabeledCommand (strOption
    (long "classifier"
    <> short 'c'
    <> metavar "CMD"
    <> help "Classifier command")
    ))
    <*> option parseMicroseconds (long "timeout" <> short 't' <> metavar "SECONDS" <> help "Timeout in seconds (default: 60)" <> value 60000000)

parseLabeledCommand :: String -> (String, String)
parseLabeledCommand s =
    -- TODO
    case break (== ':') s of
      (cmd, "") -> (cmd, cmd)
      (name, (':' : cmd)) -> (name, dropWhile (== ' ') cmd)

parseMicroseconds :: ReadM Int
parseMicroseconds = do
    t <- (auto :: ReadM (Fixed E6))
    when (t <= 0) (fail "Timeout must be positive")
    return (round (t * 1000000))

parseArgs :: IO CmdLineArgs
parseArgs = execParser opts
    where
    opts = info (parser <**> helper)
        ( fullDesc
        <> progDesc "Benchmark a set of programs on a set of files"
        <> header "ben - run benchmarks"
        )
