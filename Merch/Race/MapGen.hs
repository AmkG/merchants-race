{- Merch.Race.MapGen - Map generator integration.

Copyright 2013 Alan Manuel K. Gloria

This file is part of Merchant's Race.

Merchant's Race is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Merchant's Race is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Merchant's Race.  If not, see <http://www.gnu.org/licenses/>.
-}
module Merch.Race.MapGen
  ( MapGenM(..)
  , mapgen
  ) where

import Merch.Race.MapGen.FilterLakes
import Merch.Race.MapGen.Forest
import Merch.Race.MapGen.Island
import Merch.Race.MapGen.Measure
import Merch.Race.MapGen.Monad
import Merch.Race.MapGen.Mountain
import Merch.Race.MapGen.Settlement
import Merch.Race.MapGen.Substep

mapgen :: MapGenM m => m ()
mapgen = do
  substep 0.00 0.25 $ drawIsland
  substep 0.25 0.10 $ filterLakes
  substep 0.35 0.15 $ drawMountains
  substep 0.50 0.10 $ drawForest
  settlements <- substep 0.60 0.25 $ drawSettlement
  substep 0.85 0.15 $ measureDistances settlements
  return ()
