module Frontend.Common.UI where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Point)
import Control.Monad
import Data.Maybe

type Point = (Float, Float)
type Choice a = (a -> a)

data Viewport = Viewport Point Point
data ClickableArea a = Area Point Point (Choice a)
data Widget a = Widget [ClickableArea a] (a -> Picture)
data UI a = UI a (Widget a)
data Shell a = Shell { mouseDown :: Bool, contents :: a }

wrap a = Shell False a
unwrap (Shell _ a) = a

vpWidth (Viewport (x1,_) (x2,_)) = x2 - x1
vpHeight (Viewport (_, y1) (_, y2)) = y2 - y1

translate :: Float -> Float -> Widget a -> Widget a
translate x y (Widget areas draw) = Widget (map translate' areas) $ (Translate x y) . draw
      where translate' (Area (x1,y1) (x2,y2) c) = Area (x1+x,y1+y) (x2+x,y2+y) c

selectArea :: Point -> [ClickableArea a] -> Maybe (Choice a)
selectArea (x, y) xs = liftM getChoice $ listToMaybe $ filter (within x y) $ reverse xs
      where getChoice (Area _ _ c) = c
            within x y (Area (x1, y1) (x2, y2) _) = all id [x1 <= x, y1 <= y, x <= x2, y <= y2]

getAreas (Widget areas _) = areas
getDrawFunc (Widget _ draw) = draw

combineWidgets :: [Widget a] -> Widget a
combineWidgets xs = Widget (concat ys) $ \world -> Pictures (map (\t -> t world) zs)
      where ys = map getAreas xs
            zs = map getDrawFunc xs

handleClick :: [ClickableArea a] -> Point -> a -> a
handleClick areas p w = (fromMaybe id $ selectArea p areas) w

handleEvents :: [ClickableArea a] -> Event -> Shell a -> Shell a
handleEvents areas (EventKey (MouseButton _) Down _ (x, y)) s@(Shell _ a) = Shell True $ handleClick areas (x, y) a
handleEvents areas (EventKey (MouseButton _) Up _ _) (Shell _ a) = Shell False a
handleEvents areas (EventMotion (x, y)) (Shell True a) = Shell True $ handleClick areas (x, y) a
handleEvents _ _ w = w

handleTime = const id

runUI :: String -> Viewport -> UI a -> IO ()
runUI title vp@(Viewport (x1, y1) (x2, y2)) (UI initWorld toplevelWidget)
      = play display black 24 (wrap initWorld) (drawFunction . unwrap) eventFunction handleTime
      where display = (InWindow title (ceiling (x2-x1), ceiling (y2-y1)) (20, 20))
            eventFunction = handleEvents $ getAreas toplevelWidget
            drawFunction = getDrawFunc toplevelWidget
