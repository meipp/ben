module Tables (
    RowDescriptor(..),
    Table(..),
    centered,
    leftAligned,
    renderTable,
    rightAligned,
) where

import Data.Array
import Data.List (intercalate)

data Alignment = L | R | C

type Array1D = Array Int
type Array2D = Array (Int, Int)

array1d :: [a] -> Array1D a
array1d xs = array (1, length xs) (zip [1..] xs)

array2d :: [[a]] -> Array2D a
-- TODO
array2d xs = array ((1, 1), (x, y)) [
    ((i, j), xs !! (j-1) !! (i-1)) | i <- [1..x], j <- [1..y]
    ]
    where
        (x, y) = (length (head xs), length xs)

pad :: String -> String
pad s = " " ++ s ++ " "

align :: Alignment -> Width -> String -> String
align L n s = s ++ replicate (n - length s) ' '
align R n s = replicate (n - length s) ' ' ++ s
align C n s = replicate (n - length s - l) ' ' ++ s ++ replicate l ' '
    where
        l = (n - length s) `div` 2

data ColumnDescriptor a = ColumnDescriptor {
    columnAlignment :: Alignment,
    columnName :: String,
    generateColumnValue :: a -> String
    }

leftAligned :: String -> (a -> String) -> ColumnDescriptor a
leftAligned = ColumnDescriptor L

rightAligned :: String -> (a -> String) -> ColumnDescriptor a
rightAligned = ColumnDescriptor R

centered :: String -> (a -> String) -> ColumnDescriptor a
centered = ColumnDescriptor C

data RowDescriptor a = Header | HLine | Row a

data Table a = Table {
    header :: [ColumnDescriptor a],
    rows :: [RowDescriptor a]
    }

joinCells :: [String] -> String
joinCells cells = "|" ++ (intercalate "|" cells) ++ "|"

data RenderableCell = Dashes | Text String

type Width = Int

width :: RenderableCell -> Width
width Dashes = 0
width (Text s) = length s

widths :: Array2D RenderableCell -> Array1D Width
widths table =
    array1d [maximum [width (table ! (i, j)) | j <- [1..y]] | i <- [1..x]]
    where
        (_, (x, y)) = bounds table

prerenderTable :: Table a -> Array2D RenderableCell
prerenderTable Table{header, rows} = array2d $ map (`prerenderRow` header) rows

prerenderRow :: RowDescriptor a -> [ColumnDescriptor a] -> [RenderableCell]
prerenderRow Header = map (Text . columnName)
prerenderRow HLine = map (const Dashes)
prerenderRow (Row r) = map (Text . (`generateColumnValue` r))

renderTable :: Table a -> String
renderTable Table{header, rows} = unlines [joinCells [renderCell (columnAlignment c) (ws ! i) (p ! (i, j)) | (i, c) <- zip [1..] header] | j <- [1..y]]
    where
        p = prerenderTable Table{header, rows}
        (_, (_, y)) = bounds p
        ws = widths p

renderCell :: Alignment -> Width -> RenderableCell -> String
renderCell _ w Dashes = replicate (w + 2) '-'
renderCell a w (Text s) = pad $ align a w s
