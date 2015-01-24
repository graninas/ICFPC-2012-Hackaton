module Runtime.Emulator where

import Runtime.Types
import qualified Data.Vector as V

import Data.Maybe (isJust)

setRobot = undefined
emptyCell = undefined
openLift = undefined
setStone = undefined



getCell :: Position -> Maze -> Maybe Char
getCell (x, y) m = (V.!?) m x >>= (\m' -> (V.!?) m' y)


isRobotPassable c = c `elem` "\\. O"

isValidMove :: Position -> Maze -> Bool
isValidMove pos m = case getCell pos m of
    Just c -> isRobotPassable c
    Nothing -> False



moveRobot current@(x, y) 'L' m | isValidMove (x - 1, y) m = setRobot (x - 1, y) (emptyCell current m)
moveRobot _ _ _ = error "moveRobot not implemented."

rowHasRobot row = 'R' `V.elem` row

-- TODO: check whether x and y are valid, not flipped.
findRobot m = let
    mbRowWithRobotIdx = V.findIndex rowHasRobot m
    mbColumnWithRobotIdx = mbRowWithRobotIdx >>= (\rowIdx -> V.findIndex (=='R') (m V.! rowIdx))
    in case (mbRowWithRobotIdx, mbColumnWithRobotIdx) of
        (Just rowIdx, Just colIdx) -> (rowIdx, colIdx)
        _ -> error "invalid maze: robot not found."


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
