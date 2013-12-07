{- Merch.Race.MapGen.FilterLakes - Lake filtering algorithm.

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
{- The base island generator tends to generate
   tiny lakes.  The filterLakes function removes
   tiny lakes.  -}
module Merch.Race.MapGen.FilterLakes
  ( filterLakes
  ) where

import Merch.Race.Data
import Merch.Race.Hex
import Merch.Race.MapGen.Monad
import Merch.Race.MapGen.Substep

import Control.Monad
import Data.Ix
import Data.Ratio
import qualified Data.Set as Set
import Data.Set(Set)

filterLakes :: MapGenM m => m ()
filterLakes = do
  bounds <- mgMapBounds

  let total = fromIntegral $ rangeSize bounds

  mgStep "Removing lakes near coast"
  substep 0.00 0.10 $ forM_ (zip [1..] $ range bounds) $ \ (i,h) -> do
    mgProgress (i % total)
    t <- mgGetTerrain h
    when (t == Freshwater) $ do
      nts <- mapM mgGetTerrain $ neighbors h
      when (any (==Coast) nts) $ do
        mgPutTerrain h Plains

  mgStep "Finding small lakes"
  let findSmalls smalls []          = mgProgress 1 >> return smalls
      findSmalls smalls ((i,h):ihs) = do
        mgProgress (i % total)
        t <- mgGetTerrain h
        if (t == Freshwater)
         then do
          let ns = neighbors h
          nts <- mapM mgGetTerrain ns
          let fts = filter (==Freshwater) nts
          if length fts <= 1
           then findSmalls (Set.insert h smalls) ihs
           else findSmalls smalls                ihs
         else   findSmalls smalls                ihs
  smalls <- substep 0.10 0.45 $ findSmalls Set.empty $ zip [1..] $ range bounds

  mgStep "Removing small lakes"
  let totalsmall = fromIntegral $ Set.size smalls
  substep 0.55 0.10 $ forM_ (zip [1..] $ Set.toList smalls) $ \ (i,h) -> do
    mgProgress (i % totalsmall)
    mgPutTerrain h Plains

  mgStep "Removing tiny lakes"
  substep 0.65 0.35 $ forM_ (zip [1..] $ range bounds) $ \ (i,h) -> do
    mgProgress (i % total)
    t <- mgGetTerrain h
    when (t == Freshwater) $ do
      let ns = neighbors h
      nts <- mapM mgGetTerrain ns
      let fts = filter (==Freshwater) nts
      when (fts == []) $ do
        mgPutTerrain h Plains
