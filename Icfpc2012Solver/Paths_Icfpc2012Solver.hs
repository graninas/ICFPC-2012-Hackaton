module Paths_Icfpc2012Solver where

import System.FilePath

-- This module used by cabal-install.

dataPath = "./Data/"

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (</>) dataPath