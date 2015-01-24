module Runtime.Emulator where

import Runtime.Types

data EmulationResult = Success
                     | Fail
  deriving (Show, Read, Eq)

runEmulation :: Maze -> String -> EmulationResult
runEmulation m sol = undefined