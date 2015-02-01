module Runtime.Types where

import qualified Data.Matrix as M

type Cell = Char
type Maze = M.Matrix Char
type Position = (Int, Int)
