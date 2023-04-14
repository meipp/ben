module Tables where

import Data.Array
import Control.Monad (forM_, when)

data Alignment = L | R | C

data ColumnDescription = ColumnDescription {
    name :: String,
    alignment :: Alignment
    }

makeTable :: [ColumnDescription] -> [[String]] -> Array (Int, Int) String
makeTable header rows =
    array ((1, 1), (length header, length rows')) [
        ((i, j), rows' !! (j-1) !! (i-1)) | i <- [1..length header], j <- [1..length rows']
        ]
    where
        rows' = map name header : rows

widths :: Array (Int, Int) String -> Array Int Int
widths table =
    array1d [maximum [length (table ! (i, j)) | j <- [1..y]] | i <- [1..x]]
    where
        (_, (x, y)) = bounds table

array1d :: [a] -> Array Int a
array1d xs = array (1, length xs) (zip [1..] xs)

printTable :: Array Int Alignment -> Array (Int, Int) String -> IO ()
printTable alignments table = do
    let (_, (x, y)) = bounds table
    let ws = widths table
    forM_ [1..y] $ \j -> do
        putStr "|"
        forM_ [1..x] $ \i -> do
            putStr $ pad $ align (alignments ! i) (ws ! i) (table ! (i, j))
            putStr "|"
        putStrLn ""
        when (j == 1) $ do
            putStr "|"
            forM_ [1..x] $ \i -> do
                putStr $ replicate ((ws ! i) + 2) '-'
                putStr "|"
            putStrLn ""

pad :: String -> String
pad s = " " ++ s ++ " "

align :: Alignment -> Int -> String -> String
align L n s = s ++ replicate (n - length s) ' '
align R n s = replicate (n - length s) ' ' ++ s
align C n s = replicate (n - length s - l) ' ' ++ s ++ replicate l ' '
    where
        l = (n - length s) `div` 2
