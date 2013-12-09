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
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)

measureDistances :: MapGenM m => [(Settlement, HexCoord)] -> m ()
measureDistances settlements = do
  let total = fromIntegral $ length settlements
      h2s = Map.fromList $ map (\ (s,h) -> (h,s) ) settlements
  forM_ (zip [0..] settlements) $ \ (i, (s, h)) -> do
    mgStep $ "Measuring distances ("++show i++"/"++show total++")"
    mgProgress $ i % total
    let analyzeLoop dist done []        = return ()
        analyzeLoop dist done wavefront = do
          let daysTravel = fromIntegral $ dist `div` 4
          hss <- forM wavefront $ \ h -> do
            let ms2 = Map.lookup h h2s
            case ms2 of
              Just s2 -> mgPutDistance s s2 daysTravel
              Nothing -> return ()
            neighborM h
          let hs = Set.fromList $ concat hss
              done' = Set.union hs done
              toAdd = Set.difference hs done
              wavefront' = Set.toList toAdd
          analyzeLoop (dist+1) done' wavefront'
    ns <- neighborM h
    analyzeLoop 1 (Set.fromList $ h:ns) ns
  mgProgress 1

neighborM :: MapGenM m => HexCoord -> m [HexCoord]
neighborM h = filterM mgGetRoad $ neighbors h
