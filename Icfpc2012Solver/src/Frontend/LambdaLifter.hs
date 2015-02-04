import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import qualified Data.Matrix as M
import Control.Monad
import Data.Maybe
import Data.Char
import System.Environment
import Frontend.Common
import Runtime.Types
import Runtime.Emulator
import Debug.Trace

renderMaze :: Resources -> Maze -> Picture
renderMaze resources m = pictures $ explore m placeObject
   where placeObject chr (y, x) = Translate
               (fromIntegral x*32 - biasX)
               (biasY - fromIntegral y*32)
               (fromJust $ lookup chr resources)
         (y, x) = (M.ncols m, M.nrows m)
         (biasY, biasX) = ((fromIntegral y) * 32 / 2 - 16, (fromIntegral x) * 32 / 2 - 16) -- Picture needs to be centered

performStep :: Event -> Maze -> Maze
performStep (EventKey (Char c) Up _ _) m = trace (show c) $ runEmulation m (toUpper c)
performStep _ m = m

main = do
   resources <- loadResources
   fname <- fname `fmap` parseCmdOpts
   maze <- loadMaze fname
   play (InWindow "Lambda Lifter" (640, 480) (20, 20)) black 24 maze (renderMaze resources) performStep (const id)
