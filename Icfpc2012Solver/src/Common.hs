module Common where

import Paths_Icfpc2012Solver as P

mazesPath :: FilePath
mazesPath = "/Mazes/"

testDataPath :: FilePath
testDataPath = "/TestData/"

loadMazeFile :: FilePath -> IO String
loadMazeFile file = getDataFileName (mazesPath ++ file) >>= readFile

loadTestData :: FilePath -> IO String
loadTestData file = getDataFileName (testDataPath ++ file) >>= readFile