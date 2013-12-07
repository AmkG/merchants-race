{- Merch.Race.MapGen.PerlinSpread - Spread a terrain using perlin noise.

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
{- Spread out a particular terrain on the map, using a
   perlin function to determine probability of spreading
   onto some hex.  -}
module Merch.Race.MapGen.PerlinSpread
  ( perlinSpread
  ) where

import Merch.Race.Data
import Merch.Race.Hex
import Merch.Race.MapGen.Monad

import Control.Monad
import Data.Ratio
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(EmptyL, (:<)), (|>), (><))
import qualified Data.Set as Set
import Data.Set (Set)
import Math.Noise
import Math.Noise.Modules.Perlin

perlinModule :: Int -> Perlin
perlinModule = Perlin
  2.0 -- Frequency
  2.0 -- Lacunarity (frequency multiplier between successive octaves)
  5 -- number of octaves
  0.8 -- Persistence (amplitude multiplier between successive octaves)

perlinFunc :: Int -> (Double,Double) -> Double
perlinFunc seed = func
 where
  mod = perlinModule seed
  func (x,y) = case getValue mod (x,y,0) of
                 Just r  -> abs r
                 Nothing -> 0

perlinSpread :: MapGenM m
             => Int --                   Seed
             -> (HexCoord, HexCoord) --  Map boundaries
             -> Integer --               Maximum number
             -> (HexCoord -> m Bool) --  Filter
             -> Terrain --               Terrain to paint
             -> [HexCoord] --            Starting locations
             -> m ()
perlinSpread seed (lb,ub) total pred t hs = do
  let perlinRaw = perlinFunc seed

      -- Create a noise function from hex coords to
      -- a number between 0 and 0.5

      -- map 0,0 to 1,1 on a slightly larger grid.
      (lx, ly) = toOffset lb
      (hx, hy) = toOffset ub
      superlb = fromOffset (lx-1,ly-1)
      superub = fromOffset (hx+1,hy+1)
      (lcx, lcy) = position superlb
      (ucx, ucy) = position superub
      (diffx, diffy) = (ucx - lcx, ucy - lcy)

      -- Create the actual noise function.
      perlin :: HexCoord -> Double
      perlin h = case position h of
        (x, y) -> perlinRaw ((x - lcx) / diffx, (y - lcy) / diffy)

      -- The draw loop
      draw i q done maxp
        | i == total = return ()
        | otherwise  = case Seq.viewl q of
          EmptyL -> return ()
          h :< q -> do
            mgProgress (i % total)
            let ph = perlin h
                maxp'
                  | ph > maxp = ph
                  | otherwise = maxp
            r <- mgRandom
            if r * maxp' <= ph
             then do
              let i' = i + 1
              mgPutTerrain h t
              -- Put neighboring candidate tiles as future land candidates
              candidates <- filterM pred $ neighbors h
              let candidates' = filter (not . flip Set.member done) candidates
                  done' = Set.union (Set.fromList candidates') done
                  q' = q >< Seq.fromList candidates'
              draw i' q' done' maxp'
             else do
              draw i (q |> h) done maxp'

  draw 0 (Seq.fromList hs) Set.empty 0.0
