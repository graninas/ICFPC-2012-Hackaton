module Runtime.Emulator where

import Runtime.Types
import qualified Data.Vector as V

setRobot = undefined
emptyCell = undefined
openLift = undefined
setStone = undefined

isValidMove = undefined



moveRobot current@(x, y) 'L' m | isValidMove (x - 1, y) = setRobot (x - 1, y) (emptyCell current m)
moveRobot _ _ _ = error "Not implemented."

rowHasRobot = undefined

findRobot m = let
    rowWithRobotIdx = V.findIndex rowHasRobot m
    
    in undefined


runEmulation :: Maze -> Char -> Maze
runEmulation m 'L' = moveRobot (findRobot m) 'L' m
runEmulation m 'R' = moveRobot (findRobot m) 'R' m
runEmulation m 'U' = moveRobot (findRobot m) 'U' m
runEmulation m 'D' = moveRobot (findRobot m) 'D' m

{-
Move left, L, moving the Robot from (x, y) to (x − 1, y).
• Move right, R, moving the Robot from (x, y) to (x + 1, y).
• Move up, U, moving the Robot from (x, y) to (x, y + 1).
• Move down, D, moving the Robot from (x, y) to (x, y − 1).
• Wait, W, which does nothing.
• Abort, A, which abandons mine exploration.

-}