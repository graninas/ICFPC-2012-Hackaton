module Common where

import Paths_Icfpc2012Solver as P

mazesPath :: FilePath
mazesPath = "/Mazes/"

testDataPath :: FilePath
testDataPath = "/TestData/"

loadMazeFile :: FilePath -> IO String
loadMazeFile file = getDataFileName (mazesPath ++ file) >>= readFile

type TestFixture = String

loadTestData :: TestFixture -> FilePath -> IO String
loadTestData testFixture file = getDataFileName (testDataPath ++ testFixture ++ "/" ++ file) >>= readFile
