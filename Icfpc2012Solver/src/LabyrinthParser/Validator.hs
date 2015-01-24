module LabyrinthParser.Validator where

import Runtime.Types
import qualified Data.Vector as V

validateOneRobot :: Maze -> Either String Maze
validateOneRobot m = case V.foldl' robotsCount 0 m of
    0 -> Left "No robots."
    1 -> Right m
    n -> Left $ "Too many robots: " ++ show n
  where
    robotsCount n row = V.foldl' checkRobot n row
    checkRobot n 'R' = n + 1
    checkRobot n _   = n

validate maze = validateOneRobot maze
            -- >>= validateOneClosedLift