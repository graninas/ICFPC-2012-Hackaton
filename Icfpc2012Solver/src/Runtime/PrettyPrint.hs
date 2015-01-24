module Runtime.PrettyPrint where

import Runtime.Types
import qualified Data.Vector as V


toList :: Maze -> [String]
toList = map V.toList . V.toList
