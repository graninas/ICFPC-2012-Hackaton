module Main where

import Common
import LabyrinthParser.Parser
import Runtime.Emulator

import Control.Monad (liftM)

import qualified Testing.ParserTest as T1
import qualified Testing.EmulatorTest as T2


main :: IO ()
main = do

    T1.test
    T2.test

    m001Data <- loadMazeFile "001.txt"
    let m001 = parse m001Data
    let solution = "DDDLLLLLLURRRRRRRRRRRRDDDDDDDLLLLLLLLLLLDDDRRRRRRRRRRRD"
    
    --TODO: initial structure
    case m001 of
        Left msg -> putStrLn msg
        Right maze001 -> do
            let m001_1 = runEmulation maze001 'L'
            
            -- TODO
            putStrLn "Emulation done."
    
    putStrLn "Done."
