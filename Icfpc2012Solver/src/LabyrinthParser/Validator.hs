module LabyrinthParser.Validator where

import Runtime.Types
import qualified Data.Vector as V

validateOneObject :: Char -> String -> Maze -> Either String Maze
validateOneObject obj objname m = case V.foldl' robotsCount 0 m of
    0 -> Left $ "No " ++ objname ++ "."
    1 -> Right m
    n -> Left $ "Too many " ++ objname ++ ":" ++ show n
  where
    robotsCount n row = V.foldl' checkRobot n row
    checkRobot n obj' | obj == obj' = n + 1
    checkRobot n _   = n


validateOneRobot :: Maze -> Either String Maze
validateOneRobot = validateOneObject 'R' "robots"

validateOneClosedLift :: Maze -> Either String Maze
validateOneClosedLift = validateOneObject 'L' "lifts"

validate maze = validateOneRobot maze
            >>= validateOneClosedLift
