module Frontend.Common where

import System.Environment
import System.Exit

import Graphics.Gloss
import Control.Monad
import Runtime.Types
import LabyrinthParser.Parser as P
import Paths_Icfpc2012Solver as P

type Resources = [(Char, Picture)]

data CmdOpts = CmdOpts { fname :: String }
  deriving Show

loadMaze :: String -> IO Maze
loadMaze fname = (either error id . P.parse) `fmap` readFile fname

loadResources :: IO [(Char, Picture)]
loadResources = liftM2 zip (return ascii) $ mapM (loadBMP <=< P.getDataFileName) images
   where ascii = ['R','*','L','.','#','\\','O',' ']
         images = map ((++) "Images/") ["Robot.bmp", "Rock.bmp", "ClosedLift.bmp", "Earth.bmp",
                   "Wall.bmp", "Lambda.bmp", "OpenLift.bmp", "Empty.bmp"]

printHelpAndExit :: IO a
printHelpAndExit = do
   arg0 <- getProgName
   putStrLn ("Usage: " ++ arg0 ++ " <filename>")
   exitFailure >> undefined

parseCmdOpts :: IO CmdOpts
parseCmdOpts = do
   args <- getArgs
   if (length args) == 1 then return (CmdOpts $ head args) else printHelpAndExit
