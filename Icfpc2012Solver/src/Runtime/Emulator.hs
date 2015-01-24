module Runtime.Emulator where

import Runtime.Types
import qualified Data.Vector as V

import Data.Maybe (isJust)

-- TODO: we need a constants for symbols.

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
moveRobot current@(x, y) 'R' m | isValidMove (x + 1, y) m = setRobot (x + 1, y) (emptyCell current m)
moveRobot current@(x, y) 'U' m | isValidMove (x, y - 1) m = setRobot (x, y - 1) (emptyCell current m)
moveRobot current@(x, y) 'D' m | isValidMove (x, y + 1) m = setRobot (x, y + 1) (emptyCell current m)

moveRobot _ move _ | not $ move `elem` "LRUD" = error $ "Command is not Robot's move: " ++ [move]
moveRobot _ _ m = m

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
runEmulation m 'W' = m
runEmulation m 'A' = error "Abort is undefined."

