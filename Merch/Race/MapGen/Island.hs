{- Merch.Race.MapGen.Island - Creates the island.

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
{- Initially draws the island.  -}
module Merch.Race.MapGen.Island
  ( drawIsland
  ) where

import Merch.Race.Data
import Merch.Race.Hex
import Merch.Race.MapGen.Monad
import Merch.Race.MapGen.PerlinSpread
import Merch.Race.MapGen.Substep

import Control.Monad
import Data.Ix
import Data.List
import Data.Ratio
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Sequence(Seq, ViewL(EmptyL, (:<)), (|>), (><))
import qualified Data.Sequence as Seq

landTotalRatio :: Rational
landTotalRatio = 0.4

drawIsland :: MapGenM m => m ()
drawIsland = do

  -- Get map bounds
  (lb, ub) <- mgMapBounds
  let totaltiles = rangeSize (lb, ub)
      landtiles = (totaltiles *
                   (fromIntegral $ numerator landTotalRatio))
            `div` (fromIntegral $ denominator landTotalRatio)

  -- Clear the map to freshwater.  20%
  mgStep "Clearing the map to freshwater"
  forM_ (zip [1..] $ range (lb, ub)) $ \ (i, h) -> do
    mgProgress $ (fromIntegral i % fromIntegral totaltiles) * 0.20
    mgPutTerrain h Freshwater
    mgPutRoad h False

  -- Perform the actual drawing of the island. 50%
  mgStep "Rendering island"
  let isWater h
        | inRange (lb,ub) h = mgGetTerrain h >>= return . (== Freshwater)
        | otherwise         = return False
      ilandtiles = fromIntegral landtiles

      (lx, ly) = toOffset lb
      (hx, hy) = toOffset ub

      (cx, cy) = ((lx + hx) `div` 2, (ly + hy) `div` 2)
      center = fromOffset (cx, cy)
  seed <- mgRandom
  substep 0.2 0.5 $
    perlinSpread seed (lb,ub) ilandtiles isWater Plains [center]
  mgProgress 0.7

  -- Flood fill the seas starting from the lb corner.  25%
  mgStep "Filling seas"
  let watertiles = fromIntegral $ totaltiles - landtiles
      flood i h toget = do
        mgProgress $ 0.70 + (i % watertiles) * 0.30
        mgPutTerrain h Sea
        mgPutRoad h False
        forM_ (neighbors h) $ \n -> do
          t <- mgGetTerrain n
          when (t == Plains) $ mgPutTerrain n Coast
        toadd <- filterM isWater $ neighbors h
        let ntoget = foldl' (flip Set.insert) toget toadd
            viewntoget = Set.minView ntoget
        case viewntoget of
          Just (h,nntoget) -> flood (i + 1) h nntoget
          Nothing          -> return ()
      lbo = toOffset lb
      ubo = toOffset ub
      cornerso = [lbo, ubo, (fst lbo, snd ubo), (fst ubo, snd lbo)]
      corners = map fromOffset cornerso
  flood 1 (head corners) (Set.fromList $ tail corners)

  -- Finish
  mgProgress 1
