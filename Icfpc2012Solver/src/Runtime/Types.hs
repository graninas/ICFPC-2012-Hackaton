module Runtime.Types where

import qualified Data.Vector as V

type Cell = Char
type MazeRow = V.Vector Cell
type Maze = V.Vector MazeRow



type Position = (Int, Int)

