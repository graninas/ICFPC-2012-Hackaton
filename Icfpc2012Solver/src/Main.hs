module Main where

import Common
import qualified Testing.ParserTest as T1 


main::IO()
main = do

    T1.test

    m001 <- loadMaze "001.txt"
    

    putStrLn "Done."