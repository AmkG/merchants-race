{- Merch.Race.MapGen.Measure - Measure distances between settlements.

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

module Merch.Race.MapGen.Measure
  ( measureDistances
  ) where

import Merch.Race.Data
import Merch.Race.Data.TMap
import Merch.Race.Hex
import Merch.Race.MapGen.Monad

import Control.Monad
import Data.Graph.AStar
import Data.Ratio
import Data.Set(Set)
import qualified Data.Set as Set

measureDistances :: MapGenM m => [(Settlement, HexCoord)] -> m ()
measureDistances settlements = do
  let sPairs = pairs settlements
      total = fromIntegral $ length sPairs
  forM_ (zip [0..] sPairs) $ \ (i, ((s1, h1), (s2, h2))) -> do
    mgStep $ "Measuring distances ("++show i++"/"++show total++")"
    mgProgress $ i % total
    Just path <- aStarM
                   neighborM
                   (const $ const $ return 1)
                   (return . fromIntegral . distance h2)
                   (return . (==h2))
                   (return h1)
    let distance = fromIntegral $ length path
        daysTravel = distance `div` 4
    mgPutDistance s1 s2 daysTravel
  mgProgress 1

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (a:as) = gen a as ++ pairs as
 where
  gen _ []     = []
  gen a (b:bs) = (a,b):gen a bs

neighborM :: MapGenM m => HexCoord -> m (Set HexCoord)
neighborM h = do
  ns <- filterM mgGetRoad $ neighbors h
  return $ Set.fromList ns
