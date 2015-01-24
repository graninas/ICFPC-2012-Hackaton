module Runtime.Emulator where
import Data.Maybe

import Runtime.Types
import qualified Data.Vector as V

updateCell :: Char -> Position -> Maze -> Maze
updateCell c (x, y) m = V.update m $ V.fromList [(y, row')]
    where
        row' = V.update row $ (V.fromList [(x, '@')])
        row = V.unsafeIndex m x

setRobot = updateCell 'R'
emptyCell = updateCell ' '
openLift m = updateCell 'O' (findObject m 'L') m
setStone = updateCell '*'

isValidMove = undefined

moveRobot :: Position -> Char -> Maze -> Maze
moveRobot current@(x, y) 'L' m | isValidMove (x - 1, y) = setRobot (x - 1, y) (emptyCell current m)
moveRobot _ _ _ = error "Not implemented."

rowHasRobot = V.elem '@'

findObject :: Maze -> Char -> Position
findObject m c = let
    rowIdx = fromJust $ V.findIndex (V.elem c) m
    row = V.unsafeIndex m rowIdx
    isObject = (==) c
    in (rowIdx, fromJust $ V.findIndex isObject row) 

findRobot m = let
    in undefined


runEmulation :: Maze -> Char -> Maze
runEmulation m 'L' = moveRobot (findRobot m) 'L' m
runEmulation m 'R' = moveRobot (findRobot m) 'R' m
runEmulation m 'U' = moveRobot (findRobot m) 'U' m
runEmulation m 'D' = moveRobot (findRobot m) 'D' m

{-
Move left, L, moving the Robot from (x, y) to (x − 1, yMaze).
• Move right, R, moving the Robot from (x, y) to (x + 1, y).
• Move up, U, moving the Robot from (x, y) to (x, y + 1).
• Move down, D, moving the Robot from (x, y) to (x, y − 1).
• Wait, W, which does nothing.
• Abort, A, which abandons mine exploration.

-}
