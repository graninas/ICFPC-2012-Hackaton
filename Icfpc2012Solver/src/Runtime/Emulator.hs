module Runtime.Emulator where
import Data.Maybe

import Runtime.Types
import qualified Data.Matrix as M

import Data.Maybe (isJust)

-- TODO: we need a constants for symbols.

updateCell :: Char -> Position -> Maze -> Maze
updateCell = M.setElem

setRobot = updateCell 'R'
emptyCell = updateCell ' '
openLift m = updateCell 'O' (findObject 'L' m) m
setStone = updateCell '*'

mapRows :: Maze -> (Cell -> Position -> a) -> [[a]]
mapRows m f = map mapRow [1..M.nrows m]
    where mapRow row = map (\col -> f (m M.! (row, col)) (row, col)) [1..M.ncols m]

explore m f = concat $ mapRows m f

printMaze :: Maze -> String
printMaze m = unlines $ mapRows m (flip $ const id)

getCell :: Position -> Maze -> Maybe Char
getCell (x, y) m
    | x < M.ncols m && y < M.ncols m = Just $ m M.! (x, y)
    | otherwise                      = Nothing

isRobotPassable c = c `elem` "\\. O"

isValidMove :: Position -> Maze -> Bool
isValidMove pos m = case getCell pos m of
    Just c -> isRobotPassable c
    Nothing -> False

findObject :: Char -> Maze -> Position
findObject c m = head $ catMaybes $ explore m testCell
    where testCell c1 (col, row) = if c1== c then Just (col, row) else Nothing

moveRobot current@(x, y) 'L' m | isValidMove (x, y - 1) m = setRobot (x, y - 1) (emptyCell current m)
moveRobot current@(x, y) 'R' m | isValidMove (x, y + 1) m = setRobot (x, y + 1) (emptyCell current m)
moveRobot current@(x, y) 'U' m | isValidMove (x - 1, y) m = setRobot (x - 1, y) (emptyCell current m)
moveRobot current@(x, y) 'D' m | isValidMove (x + 1, y) m = setRobot (x + 1, y) (emptyCell current m)

moveRobot _ move _ | not $ move `elem` "LRUD" = error $ "Command is not Robot's move: " ++ [move]
moveRobot _ _ m = m

findRobot = findObject 'R'

runEmulation :: Maze -> Char -> Maze
runEmulation m 'L' = moveRobot (findRobot m) 'L' m
runEmulation m 'R' = moveRobot (findRobot m) 'R' m
runEmulation m 'U' = moveRobot (findRobot m) 'U' m
runEmulation m 'D' = moveRobot (findRobot m) 'D' m
runEmulation m 'W' = m
runEmulation m 'A' = error "Abort is undefined."

{-
Move left, L, moving the Robot from (x, y) to (x − 1, yMaze).
• Move right, R, moving the Robot from (x, y) to (x + 1, y).
• Move up, U, moving the Robot from (x, y) to (x, y + 1).
• Move down, D, moving the Robot from (x, y) to (x, y − 1).
• Wait, W, which does nothing.
• Abort, A, which abandons mine exploration.

-}
{-
runBunchEmulation :: Maze -> String -> Maze
runBunchEmulation m [] = m
runBunchEmulation m (s_head : s_tail) = let
    new_m = (runEmulation m head)
    in new_m : (runBunchEmulation new_m tail)
-}
