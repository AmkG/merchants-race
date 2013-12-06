
{- Handles mountain and hill generation.  -}
module Merch.Race.MapGen.Mountain
  ( drawMountains
  ) where

import Merch.Race.Data
import Merch.Race.Hex
import Merch.Race.MapGen.Monad
import Merch.Race.MapGen.PerlinSpread
import Merch.Race.MapGen.Substep

import Control.Monad
import Data.Ix
import Data.Ratio

mountainTotalRatio :: Rational
mountainTotalRatio = 0.08

drawMountains :: MapGenM m => m ()
drawMountains = do
  (lb, ub) <- mgMapBounds

  mgStep "Generating Mountains"
  seed <- mgRandom
  -- setup mountain generation
  let total = fromIntegral $ rangeSize (lb, ub)
      mountains = (total * numerator mountainTotalRatio)
            `div` denominator mountainTotalRatio
      check h
        | inRange (lb,ub) h = do
          t <- mgGetTerrain h
          if t == Plains
           then do
             nts <- mapM mgGetTerrain $ neighbors h
             if any (flip any [Freshwater, Coast, Sea] . (==)) nts
              then return False
              else return True
           else return False
        | otherwise       = return False
      (lx,ly) = toOffset lb
      (hx,hy) = toOffset ub
      (dx,dy) = (hx - lx, hy - ly)
  starts <- forM [1..3] $ \_ -> do
    let find = do
          rx <- mgRandom
          ry <- mgRandom
          let x = (abs rx `mod` dx) + lx
              y = (abs ry `mod` dy) + ly
              h = fromOffset (x,y)
          b <- check h
          if b then return h else find
    find
  substep 0.0 0.5 $ perlinSpread seed (lb,ub) mountains check Mountain starts
