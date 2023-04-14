module CmdLine where

import Options.Applicative

data CmdLineArgs = CmdLineArgs {
    programs :: [(String, String)],
    files :: FilePath,
    classifiers :: [(String, String)]
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

parseLabeledCommand :: String -> (String, String)
parseLabeledCommand s =
    -- TODO
    case break (== ':') s of
      (cmd, "") -> (cmd, cmd)
      (name, (':' : cmd)) -> (name, dropWhile (== ' ') cmd)

parseArgs :: IO CmdLineArgs
parseArgs = execParser opts
    where
    opts = info (parser <**> helper)
        ( fullDesc
        <> progDesc "Benchmark a set of programs on a set of files"
        <> header "ben - run benchmarks"
        )
