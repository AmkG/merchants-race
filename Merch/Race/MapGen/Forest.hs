{- Merch.Race.MapGen.Forest - Draws forests all over the map.

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
module Merch.Race.MapGen.Forest
  ( drawForest
  ) where

import Merch.Race.Data
import Merch.Race.Hex
import Merch.Race.MapGen.Monad
import Merch.Race.MapGen.PerlinSpread

import Control.Monad
import Data.Ix
import Data.Ratio

forestTotalRatio :: Rational
forestTotalRatio = 0.09

drawForest :: MapGenM m => m ()
drawForest = do
  (lb,ub) <- mgMapBounds
  -- Compute bounds.
  let (lx, ly) = toOffset lb
      (ux, uy) = toOffset ub
      (dx, dy) = (ux - lx, uy - ly)
  -- Compute number of tiles
  let total = fromIntegral $ rangeSize (lb,ub)
      forests = total * numerator forestTotalRatio
          `div` denominator forestTotalRatio
  -- Compute allowed locations.
  let check h = do
        t <- mgGetTerrain h
        if t == Plains
         then do
           -- Forests are not allowed within 2 tiles
           -- of coast.
           nts <- mapM mgGetTerrain $ nearby 2 h
           return $ not $ any (==Coast) nts
         else return False
      find = do
        xr <- mgRandom
        yr <- mgRandom
        let x = ((xr `mod` dx) + lx)
            y = ((yr `mod` dy) + ly)
            h = fromOffset (x,y)
        t <- check h
        if t then return h else find

  -- Select starting locations.
  starts <- forM [1..13] $ \_ -> find

  -- Render
  mgStep "Spreading Forests"
  seed <- mgRandom
  perlinSpread seed (lb,ub) forests check Forest starts
