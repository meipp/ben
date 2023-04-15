module Analysis where

import Measurement
import Data.Function (on)
import Data.List (group, groupBy, sort, sortOn)
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Numeric (showFFloat)
import Tables

data Report = Report {
    program' :: String,
    time' :: Int,
    errors' :: Int,
    timeouts' :: Int,
    classifications' :: [(String, Int)]
    } deriving (Show)

analyze :: [String] -> [Measurement] -> IO ()
analyze classifiers measurements = do
    let rs = map (\ms -> report (program (head ms)) ms) byProgram -- ++ [report "All" measurements]
    let table = Table {
        header =
            [leftAligned "Program" program']
            ++ map (\c -> rightAligned c (\r -> show (fromMaybe 0 (c `lookup` classifications' r)))) classifiers ++
            [
                rightAligned "Timeouts" (show . timeouts'),
                rightAligned "Errors" (show . errors'),
                rightAligned "Time (ms)" (show . time')
            ],
        rows = [Header, HLine] ++ map Row rs
        }
    putStr (renderTable table)
    where
        byProgram :: [[Measurement]]
        byProgram = groupBy ((==) `on` program) $ sortOn program $ measurements

report :: String -> [Measurement] -> Report
report label measurements = Report {
    program' = label,
    time' = sum (map time measurements),
    errors' = length (filter (\m -> case status m of
        Failure _ -> True
        _ -> False
        ) measurements),
    timeouts' = length (filter ((== Timeout) . status) measurements),
    classifications' = count (concat (map classifications measurements))
    }

count :: Ord a => [a] -> [(a, Int)]
count = map (\xs -> (head xs, length xs)) . group . sort
