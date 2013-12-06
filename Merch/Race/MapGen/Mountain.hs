
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
import Data.Graph.AStar
import Data.Ix
import Data.Ratio
import qualified Data.Set as Set

mountainTotalRatio :: Rational
mountainTotalRatio = 0.05

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
      half_dx = dx `div` 2
  -- Generate a starting seed mountain range.
  let find side = do
        rx <- mgRandom
        ry <- mgRandom
        let x | side      = ((abs rx `mod` dx) `div` 2) + lx
              | otherwise = ((abs rx `mod` dx) `div` 2) + lx + half_dx
            y = (abs ry `mod` dy) + ly
            h = fromOffset (x,y)
        b <- check h
        if b then return h else find side
      makeRange = do
        -- Generate end points from the left and
        -- right side of the map.
        start <- find True
        end <- find False
        let (ex,ey) = toOffset end
        mhs <- aStarM
                 (\h -> do
                   ns <- filterM check $ neighbors h
                   return $ Set.fromList ns)
                 (const $ const $ return 1)
                 (\h -> do
                   let (hx,hy) = toOffset h
                       (dx,dy) = (abs $ ex - hx, abs $ ey -hy)
                   return $ max dx dy)
                 (return . (==end))
                 (return start)
        case mhs of
          Nothing -> makeRange
          Just hs -> return hs
  ms <- makeRange
  substep 0.0 0.5 $ perlinSpread seed (lb,ub) mountains check Mountain ms

  mgStep "Eroding Mountains to Plains"
  substep 0.5 0.1 $ do
    toRemoves <- forM (zip [0..] (range (lb,ub))) $ \ (i, h) -> do
      mgProgress $ (i % total) / 2
      t <- mgGetTerrain h
      if t == Plains
       then filterM (\h -> do
                      t <- mgGetTerrain h
                      if t == Mountain
                       then mgRandom
                       else return False)
                    (neighbors h)
       else return []
    let toRemove = concat toRemoves
        numToRemove = fromIntegral $ length toRemove
    forM (zip [0..] toRemove) $ \ (i, h) -> do
      mgProgress $ 0.5 + (i % numToRemove) / 2
      mgPutTerrain h Plains
    mgProgress 1
