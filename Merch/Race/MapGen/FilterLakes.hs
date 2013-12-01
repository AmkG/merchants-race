
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

  mgStep "Finding small lakes"
  let total = fromIntegral $ rangeSize bounds
      findSmalls smalls []          = mgProgress 1 >> return smalls
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
  smalls <- substep 0.00 0.45 $ findSmalls Set.empty $ zip [1..] $ range bounds

  mgStep "Removing small lakes"
  let totalsmall = fromIntegral $ Set.size smalls
  substep 0.45 0.10 $ forM_ (zip [1..] $ Set.toList smalls) $ \ (i,h) -> do
    mgProgress (i % totalsmall)
    mgPutTerrain h Plains

  mgStep "Removing tiny lakes"
  substep 0.55 0.45 $ forM_ (zip [1..] $ range bounds) $ \ (i,h) -> do
    mgProgress (i % total)
    t <- mgGetTerrain h
    when (t == Freshwater) $ do
      let ns = neighbors h
      nts <- mapM mgGetTerrain ns
      let fts = filter (==Freshwater) nts
      when (fts == []) $ do
        mgPutTerrain h Plains
