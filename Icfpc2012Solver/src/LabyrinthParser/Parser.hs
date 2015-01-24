module LabyrinthParser.Parser where

import Runtime.Types
import LabyrinthParser.Validator

import qualified Data.Vector as V
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

toVector rows = V.fromList $ map V.fromList rows

parse :: String -> Either String Maze
parse = validate . toVector . fromRows


