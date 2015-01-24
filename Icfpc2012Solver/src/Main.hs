module Main where

import Common

import qualified Testing.ParserTest as T1 


main::IO()
main = do

    T1.test

    m001 <- loadMazeFile "001.txt"
    let solution = "DDDLLLLLLURRRRRRRRRRRRDDDDDDDLLLLLLLLLLLDDDRRRRRRRRRRRD"
    
    
    putStrLn "Done."