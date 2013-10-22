
module Merch.Race.MapGen
  ( MapGenM(..)
  , mapgen
  , radialIslandTest
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray
import Math.Noise.NoiseModule
import Math.Noise.Modules.Perlin
import System.IO
import System.Random

class Monad m => MapGenM m

mapgen = undefined

perlinModule :: Int -> Perlin
perlinModule = Perlin
  1.0 -- Frequency
  2.0 -- Lacunarity (frequency multiplier between successive octaves)
  8 -- number of octaves
  0.3 -- Persistence (amplitude multiplier between successive octaves)

-- Apply a function to each item in the array.
eachArray :: Ix i => (a -> a) -> STArray s i a -> ST s ()
eachArray f ar = do
  bounds <- getBounds ar
  let is = range bounds
  forM_ is $ \i -> do
    a <- readArray ar i
    let b = f a
    writeArray ar i b
-- Load an array based on a function
loadArray :: Ix i => (i -> a) -> STArray s i a -> ST s ()
loadArray f ar = do
  bounds <- getBounds ar
  let is = range bounds
  forM_ is $ \i -> writeArray ar i (f i)

-- Based on code by Amit Patel <amitp@cs.stanford.edu>
-- Released under MIT license
tau :: Double
tau = 2 * pi
islandFactor :: Double
islandFactor = 1.0 -- 1.0 means no small islands, 2.0 means lots of little islands
radialIsland :: Int -> (Double, Double) -> Bool
radialIsland seed = core
 where
  gen = mkStdGen seed
  bumpsI :: Integer
  (bumpsI, gen1) = randomR (1, 6) gen
  bumps :: Double
  bumps = fromIntegral bumpsI
  startAngle :: Double
  (startAngle, gen2) = randomR (0, tau) gen1
  dipAngle :: Double
  (dipAngle, gen3) = randomR (0, tau) gen2
  dipWidth :: Double
  (dipWidth, _) = randomR (0.3, 0.9) gen3

  core (x,y) = length < r1
            || (length > r1 * islandFactor && length < r2)
   where
    angle = atan2 y x
    length = sqrt (x * x + y * y)
    dipOffsetRaw = angle - dipAngle
    dipOffsetAdjust
      | dipOffsetRaw > pi        = dipOffsetRaw - tau
      | dipOffsetRaw < negate pi = dipOffsetRaw + tau
      | otherwise                = dipOffsetRaw
    dipOffset = abs dipOffsetAdjust
    inDip = dipOffset < dipWidth
    dipSizeRaw = dipOffset / dipWidth
    dipSize = dipSizeRaw * dipSizeRaw * dipSizeRaw
    r1Raw = 0.5 + 0.4 * sin (startAngle + bumps*angle + cos ((bumps + 3) * angle))
    r2Raw = 0.7 + 0.2 * sin (startAngle + bumps*angle + sin ((bumps + 2) * angle))
    r1
      | inDip     = interpolate dipSize 0.1 r1Raw
      | otherwise = r1Raw
    r2
      | inDip     = interpolate dipSize 0.1 r2Raw
      | otherwise = r2Raw

interpolate :: RealFrac r => r -> r -> r -> r
interpolate i p0 p1 = p0 + i * (p1 - p0)

radialIslandTest :: () -> IO ()
radialIslandTest () = do
  forM_ [1,2,3,4,5] $ \ seed -> do
    let radialFunc = radialIsland seed
        width = 300
        height = 300
        halfwidth = fromIntegral width / 2
        halfheight = fromIntegral height / 2
        func (x,y) = radialFunc (xd, yd)
         where
          xd = (fromIntegral x - halfwidth) / halfwidth
          yd = (fromIntegral y - halfheight) / halfheight
        array = runSTArray $ do
                  ar <- newArray ((0,0), (width-1,height-1)) False
                  loadArray func ar
                  return ar
        grays = [if array ! (x,y) then "255" else "0" | x <- [0..width-1], y <- [0..height-1]]
        ss = map (\g -> g ++ " " ++ g ++ " " ++ g ++ "\n") grays
        fname = "radial-" ++ show seed ++ ".ppm"
    h <- openFile fname WriteMode
    hPutStr h $
      "P3\n " ++ show width ++ " " ++ show height ++ "\n255\n"
      ++ concat ss
    hClose h
