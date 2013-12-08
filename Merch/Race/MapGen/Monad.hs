{- Merch.Race.MapGen.Monad - Map generator monad operations.

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

{- Definition for map generator monad.  -}
module Merch.Race.MapGen.Monad
  ( MapGenM(..)
  ) where

import Merch.Race.Data
import Merch.Race.Hex(HexCoord)

import System.Random

class Monad m => MapGenM m where
  -- Gets the boundaries of the map.
  mgMapBounds :: m (HexCoord, HexCoord)
  -- Reads and modifies map data.
  mgGetTerrain :: HexCoord -> m Terrain
  mgPutTerrain :: HexCoord -> Terrain -> m ()
  mgGetRoad :: HexCoord -> m Bool
  mgPutRoad :: HexCoord -> Bool -> m ()
  mgAddSettlement :: Settlement -> SettlementType -> HexCoord -> m ()
  -- Indicates the start of a step in map generation.
  mgStep :: String -> m ()
  -- Indicates how much progress in the current step.
  -- Value from 0.0 to 1.0
  mgProgress :: Rational -> m ()
  -- Gets a random item.
  mgRandom :: Random r => m r
  -- Get settlement name generator.
  mgNameGenerator :: m NameGenerator
  -- Get settlement generator.
  mgSettlementGenerator :: m [(Int, SettlementType)]
  -- Get the required Terrain of a specific SettlementType
  mgRequiredTerrain :: SettlementType -> m [Terrain]
