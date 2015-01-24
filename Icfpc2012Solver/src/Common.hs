module Common where

import Paths_Icfpc2012Solver as P

mazesPath :: FilePath
mazesPath = "/Mazes/"

testDataPath :: FilePath
testDataPath = "/TestData/"

loadMaze :: FilePath -> IO String
loadMaze file = getDataFileName (mazesPath ++ file) >>= readFile

loadTestData :: FilePath -> IO String
loadTestData file = getDataFileName (testDataPath ++ file) >>= readFile