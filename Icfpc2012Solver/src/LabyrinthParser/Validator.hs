module LabyrinthParser.Validator where

import Runtime.Types
import Runtime.Emulator -- | TODO: put somewhere in another place
import qualified Data.Matrix as M

validateOneObject :: Char -> String -> Maze -> Either String Maze
validateOneObject obj objname m = case robotsCount of
    0 -> Left $ "No " ++ objname ++ "."
    1 -> Right m
    n -> Left $ "Too many " ++ objname ++ ":" ++ show n
  where
    robotsCount = sum $ map fromEnum $ explore m (\c _-> c == obj)


validateOneRobot :: Maze -> Either String Maze
validateOneRobot = validateOneObject 'R' "robots"

validateOneClosedLift :: Maze -> Either String Maze
validateOneClosedLift = validateOneObject 'L' "lifts"

validate maze = validateOneRobot maze
            >>= validateOneClosedLift
