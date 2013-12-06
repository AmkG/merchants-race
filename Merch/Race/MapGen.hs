
module Merch.Race.MapGen
  ( MapGenM(..)
  , mapgen
  ) where

import Merch.Race.MapGen.FilterLakes
import Merch.Race.MapGen.Island
import Merch.Race.MapGen.Monad
import Merch.Race.MapGen.Mountain
import Merch.Race.MapGen.Substep

mapgen :: MapGenM m => m ()
mapgen = do
  substep 0.00 0.25 $ drawIsland
  substep 0.25 0.10 $ filterLakes
  substep 0.35 0.15 $ drawMountains

