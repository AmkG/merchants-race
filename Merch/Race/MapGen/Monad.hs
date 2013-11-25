
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
  -- Indicates the start of a step in map generation.
  mgStep :: String -> m ()
  -- Indicates how much progress in the current step.
  -- Value from 0.0 to 1.0
  mgProgress :: Rational -> m ()
  -- Gets a random item.
  mgRandom :: Random r => m r
