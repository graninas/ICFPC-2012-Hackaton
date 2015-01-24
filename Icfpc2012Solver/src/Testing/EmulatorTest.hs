module Testing.EmulatorTest where

import Common
import Runtime.Emulator
import LabyrinthParser.Parser
import Data.Either
import qualified Data.Either.Unwrap as E

import Control.Monad (liftM)

testFixture = "Emulator"
getTestName tName n = tName ++ ", " ++ takeWhile (/= '.') n

testMove (src, move, expected) = do
    Right d <- liftM parse $ loadTestData testFixture src
    Right e <- liftM parse $ loadTestData testFixture expected
    let result = runEmulation d move
    if (result == e) then putStrLn $ "passed: " ++ (getTestName "testMove" src) ++ "."
                     else putStrLn $ "FAILED! " ++ (getTestName "testMove" src) ++ "."
    
test :: IO ()
test = do

    testMove ("MoveLeftFreely.txt", 'L', "MoveLeftFreely_expected.txt")
    
