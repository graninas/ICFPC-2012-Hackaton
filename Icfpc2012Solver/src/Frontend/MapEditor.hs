{-# LANGUAGE BangPatterns #-}

import Graphics.Gloss hiding (translate)
import Frontend.Common
import Frontend.Common.UI
import Control.Monad
import Data.Maybe
import Data.Char
import qualified Data.Matrix as M
import Runtime.Emulator
import Runtime.Types
import System.IO.Unsafe
import System.Environment
import Paths_Icfpc2012Solver as P

data World = World { selectedTile :: Char, maze :: Maze, filename :: String }
      deriving Show

initWorld :: String -> World
initWorld fn = World (' ') (M.matrix 30 30 (const ' ')) fn

tileSelector :: (Char, Picture) -> Widget World
tileSelector item = Widget [Area (-16,-16) (16, 16) onClick] drawFunc
      where boundingBox isSel = Color (if isSel then green else white) $ Line [(-16,16), (16,16), (16,-16), (-16,-16), (-16, 16)]
            drawFunc !world = Pictures [snd item, boundingBox (selectedTile world == fst item)]
            onClick world = world { selectedTile = fst item }

tileMenu :: Resources -> Widget World
tileMenu items = combineWidgets $ zipWith placeButton items [0..]
      where placeButton item idx = translate (fromIntegral idx * w2) 0 $ tileSelector item
            w = 32
            margin = 5
            w2 = w + margin
            c = length items `div` 2
mazeView :: Resources -> Viewport -> Widget World
mazeView res vp@(Viewport (x1,y1) (x2,y2)) = translate x1 y1 $ Widget areas drawFunc
      where areas = map (\(x:y:xs) -> Area (x, y) (x+w, y+w) (onClick (floor (x/w), floor (y/w)))) (sequence [hPoints, vPoints])
            drawFunc !world = Pictures $ (map hLine vPoints) ++ (map vLine hPoints) ++ (drawTiles $ maze world)
            drawTiles m = catMaybes $ explore m drawTile
            drawTile cell pos = liftM (putToCell pos) (lookup cell res)
            putToCell (row, col) = Translate
                  (fromIntegral (col-1)*w + 0.5*w)
                  $ maxRow*32 - (fromIntegral (row-1)*w + 0.5*w)
            onClick (col, row) w@(World tile maze _) = w { maze = updateCell tile (floor(maxRow) - row, col+1) maze }
            hLine y = Color (greyN 0.3) $ Line [(0,y), (vpWidth(vp), y)]
            vLine x = Color (greyN 0.3) $ Line [(x,0), (x, vpHeight(vp))]
            hPoints = [0,w..vpWidth(vp)]
            vPoints = [0,w..vpHeight(vp)]
            maxRow = (y2 - y1) / w
            w = 32

actionButton :: (World -> World) -> Picture -> Widget World
actionButton action icon = Widget [Area (-16,-16) (16, 16) action] drawFunc
      where boundingBox = Color white $ Line [(-16,16), (16,16), (16,-16), (-16,-16), (-16, 16)]
            drawFunc !world = Pictures [icon, boundingBox]

loadResource fn = unsafePerformIO $ loadBMP =<< P.getDataFileName ("Images/" ++ fn)

saveButton = actionButton onClick (loadResource "Save.bmp")
      where onClick !w = (unsafePerformIO $ writeFile (filename w) $ printMaze (stripMaze $ maze w)) `seq` w

stripMaze :: Maze -> Maze
stripMaze m = M.submatrix y1 y2 x1 x2 m
      where (x1, x2, y1, y2) = (minimum hScan, maximum hScan, minimum vScan, maximum vScan)
            hScan = catMaybes $ explore m (\c (x, y) -> ifNotSpace c y)
            vScan = catMaybes $ explore m (\c (x, y) -> ifNotSpace c x)
            ifNotSpace c idx = if isSpace c then Nothing else Just idx

ui :: String -> Resources -> UI World
ui fn res = UI (initWorld fn) $ combineWidgets [mazeView res viewport,
            translate 0 200 $ tileMenu res,
            translate (-280) 200 $ saveButton ]

viewport = (Viewport (-320, -240) (320, 240))

main :: IO()
main = do
   resources <- loadResources
   fname <- fname `fmap` parseCmdOpts
   runUI "MapEditor" viewport (ui fname resources)
