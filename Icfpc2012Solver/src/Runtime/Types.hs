module Runtime.Types where

import qualified Data.Vector as V


type MazeRow = V.Vector Char
type Maze = V.Vector MazeRow

