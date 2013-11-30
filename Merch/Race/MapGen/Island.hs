
{- Initially draws the island.  -}
module Merch.Race.MapGen.Island
  ( drawIsland
  ) where

import Merch.Race.Data
import Merch.Race.Hex
import Merch.Race.MapGen.Monad

import Control.Monad
import Data.Ix
import Data.List
import Data.Ratio
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Sequence(Seq, ViewL(EmptyL, (:<)), (|>), (><))
import qualified Data.Sequence as Seq
import Math.Noise
import Math.Noise.Modules.Perlin

perlinModule :: Int -> Perlin
perlinModule = Perlin
  2.0 -- Frequency
  2.0 -- Lacunarity (frequency multiplier between successive octaves)
  5 -- number of octaves
  0.8 -- Persistence (amplitude multiplier between successive octaves)

perlinFunc :: Int -> (Double, Double) -> Double
perlinFunc seed = func
 where
  mod = perlinModule seed
  func (x,y) = case getValue mod (x,y,0) of
                 Just r  -> abs r
                 Nothing -> 0

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

  -- Generate the noise function
  seed <- mgRandom
  let perlinRaw = perlinFunc seed
      -- perlin noise function
      -- normalizes the range lb, ub to 0.0, 1.0
      (lx, ly) = toOffset lb
      (hx, hy) = toOffset ub
      -- Map to a larger grid.
      superlb = fromOffset (lx-1,ly-1)
      superub = fromOffset (hx+1,hy+1)
      (lcx, lcy) = position superlb
      (ucx, ucy) = position superub
      (diffx, diffy) = (ucx - lcx, ucy - lcy)
      -- given a hex, generates a random number between 0 and 1
      perlin :: HexCoord -> Double
      perlin h = perlinRaw ((x - lcx) / diffx, (y - lcy) / diffy)
       where
        (x,y) = position h

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

      (cx, cy) = ((lx + hx) `div` 2, (ly + hy) `div` 2)
      center = fromOffset (cx, cy)

      draw i q done maxp
        | i == ilandtiles  = return ()
        | otherwise        = case Seq.viewl q of
          EmptyL -> return ()
          h :< q -> do
            let ph = perlin h
                maxp'
                  | ph > maxp = ph
                  | otherwise = maxp
            r <- mgRandom
            if r <= ph / maxp'
              then do
                -- Draw land.
                let i' = i + 1
                mgProgress $ 0.20 + (i' % ilandtiles) * 0.50
                mgPutTerrain h Plains
                -- Put nearby water tiles as future land candidates
                candidates <- filterM isWater $ neighbors h
                let candidates' = filter (not . flip Set.member done) candidates
                    done' = Set.union (Set.fromList candidates') done
                    q' = q >< Seq.fromList candidates'
                draw i' q' done' maxp'
              -- don't draw
              else do
                mgProgress $ 0.20 + (i % ilandtiles) * 0.50
                draw i (q |> h) done maxp'
  draw 0 (Seq.singleton center) Set.empty 0.0
  mgProgress 0.7

  -- Flood-fill the seas.
  -- First fill the edge tiles with freshwater. 5%
  mgStep "Terminating land"
  let edgetiles = [fromOffset (x, y) | y <- [ly..hy], x <- [lx, hx]]
               ++ [fromOffset (x, y) | x <- [lx..hx], y <- [ly, hy]]
      numedgetiles = fromIntegral $ length edgetiles
  forM_ (zip [1..] edgetiles) $ \ (i, h) -> do
    mgProgress $ 0.7 + (i % numedgetiles) * 0.05
    mgPutTerrain h Freshwater
    mgPutRoad h False

  -- Flood fill the seas starting from the lb corner.  25%
  mgStep "Filling seas"
  let watertiles = fromIntegral $ totaltiles - landtiles
      flood i h toget = do
        mgProgress $ 0.75 + (i % watertiles) * 0.25
        mgPutTerrain h Sea
        mgPutRoad h False
        toadd <- filterM isWater $ neighbors h
        let ntoget = foldl' (flip Set.insert) toget toadd
            viewntoget = Set.minView ntoget
        case viewntoget of
          Just (h,nntoget) -> flood (i + 1) h nntoget
          Nothing          -> return ()
  flood 1 lb Set.empty

  -- Finish
  mgProgress 1
