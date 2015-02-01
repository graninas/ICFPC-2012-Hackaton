module LabyrinthParser.Parser where

import Runtime.Types
import LabyrinthParser.Validator

import qualified Data.Matrix as M
import qualified Data.List as L (foldl')

parseDimensions :: [String] -> (Int, Int)
parseDimensions rows = (L.foldl' maxColumn 0 rows, length rows)
   where
    maxColumn n r | length r >= n = length r
                  | otherwise = n

padRows :: [String] -> [String]
padRows rows = let
    (m, n) = parseDimensions rows
    pad k r = r ++ replicate (k - length r) ' '
    in map (pad m) rows

fromRows = padRows . lines

parse :: String -> Either String Maze
parse = validate . M.fromLists . fromRows


