module Testing.ParserTest where

import Common
import LabyrinthParser.Parser
import Data.Either
import qualified Data.Either.Unwrap as E

import Control.Monad (liftM)


testFixture = "Parser"
getTestName tName n = tName ++ ", " ++ takeWhile (/= '.') n

testPadding (src, expected) = do
    d <- loadTestData testFixture src
    e <- liftM lines $ loadTestData testFixture expected
    let result = fromRows d
    if (result == e) then putStrLn $ "passed: " ++ (getTestName "testPadding" src) ++ "."
                     else putStrLn $ "FAILED! " ++ (getTestName "testPadding" src) ++ ". Expected:\n" ++ show e ++ "\nResult:\n" ++ show result

testParsing (src, expected) = do
    d <- loadTestData testFixture src
    let result = parse d
    putStrLn $ (if ((E.isRight result && expected) || (E.isLeft result && not expected)) then "passed: " else "FAILED! ")
             ++ getTestName "testPadding" src ++ ", result: " ++
        case result of
            Right _ -> "parsed successfully."
            Left msg -> "not parsed. Message: " ++ msg
    
test :: IO ()
test = do

    testPadding ("OneLine.txt", "OneLine.txt")
    testPadding ("TwoLines.txt", "TwoLines.txt")
    testPadding ("TwoNonPaddedLines.txt", "TwoNonPaddedLines_expected.txt")
    
    testParsing ("OneLine.txt", True)
    testParsing ("TwoLines.txt", True)
    testParsing ("TwoNonPaddedLines.txt", True)
    testParsing ("NoRobot.txt", False)
    testParsing ("NoLift.txt", False)
    testParsing ("2Robots.txt", False)
    testParsing ("2Lifts.txt", False)
    
