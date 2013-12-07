{- Merch.Race.Hex - Hex coordinate system algorithms.

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

{- Hex coordinate system.

The hex coordinate system is the "axial" coordinate
system of Amit Patel, from:
   http://www.redblobgames.com/grids/hexagons/#coordinates

We are using "flat-topped" hexes, like so:
    ____
   /    \
  /      \
  \      /
   \____/

HexCoord's are also Ix.  An array using HexCoords
as Ix will be a rectangular section:
     ____        ____
    /    \      /    \
   / lower\____/      \
   \ bound/    \      /
    \____/      \____/
    /    \      /    \
   /      \____/      \
   \      /    \      /
    \____/      \____/
    /    \      /    \
   /      \____/ upper\
   \      /    \ bound/
    \____/      \____/
         \      /
          \____/

Hexes are defined by X and Z coordinates.  These
increase like so:

     ____        ____
    /    \      /    \
   / X = 0\____/      \
   \ Z = 0/    \      /
    \____/ X = 1\____/
    /    \      /    \
   /      \____/ X = 2\
   \ Z = 1/    \      /
    \____/      \____/
    /    \      /    \
   /      \____/      \
   \ Z = 2/    \      /
    \____/      \____/
         \      /
          \____/

-}

module Merch.Race.Hex
  ( HexCoord(..)
  , neighbors
  , nearby
  , distance

  , position
  , fromPosition

  , toOffset
  , fromOffset
  ) where

import Merch.Race.Data.Serialize

import Data.Array

newtype HexCoord
  = HexCoord (Int, Int)
  deriving (Show, Read, Eq)

toCubic :: HexCoord -> (Int, Int, Int)
toCubic (HexCoord (x,z)) = (x, negate (x + z), z)
fromCubic :: (Int, Int, Int) -> HexCoord
fromCubic (x, _, z) = HexCoord (x,z)

-- odd-q offset
toOffset :: HexCoord -> (Int, Int)
toOffset (HexCoord (x,z)) = (q, r)
 where
  q = x
  r = z + ((x - (oddv q)) `div` 2)
fromOffset :: (Int, Int) -> HexCoord
fromOffset (q, r) = HexCoord (x, z)
 where
  x = q
  z = r - ((q - (oddv q)) `div` 2)

oddv :: Int -> Int
oddv i
  | odd i     = 1
  | otherwise = 0

instance Ord HexCoord where
  compare h1 h2 =
    case compare r1 r2 of
      EQ  -> compare q1 q2
      oth -> oth
   where
    (q1, r1) = toOffset h1
    (q2, r2) = toOffset h2
instance Ix HexCoord where
  range (l, h) = map fromOffset $ range (toOffset l, toOffset h)
  index (l, h) i = index (toOffset l, toOffset h) (toOffset i)
  inRange (l, h) i = inRange (toOffset l, toOffset h) (toOffset i)
instance Serialize HexCoord where
  hPut = hPutConvert $ \ (HexCoord x) -> x
  hGet = hGetConvert $ \ x            -> (HexCoord x)

{- Returns a list of hex coordinates directly
   adjacent to the given hex.  Given the
   direction x is to the lower right and
   z is downwards, the neighbors are in
   counter-clockwise order.  -}
neighbors :: HexCoord -> [HexCoord]
neighbors (HexCoord (x,z)) =
  [ HexCoord (xp1, z  )
  , HexCoord (xp1, zm1)
  , HexCoord (x  , zm1)
  , HexCoord (xm1, z  )
  , HexCoord (xm1, zp1)
  , HexCoord (x  , zp1)
  ]
 where
  xp1 = x + 1
  xm1 = x - 1
  zp1 = z + 1
  zm1 = z - 1

{- Returns a list of hex coordinates within n
   hexes of the given hex, including the given
   hex.  -}
nearby :: Integral i => i -> HexCoord -> [HexCoord]
nearby ni (HexCoord (x,z)) = do
  let n = fromIntegral ni
  deltaX <- [negate n..n]
  deltaY <- [max(negate n)(negate deltaX-n)..min(n)(negate deltaX+n)]
  let deltaZ = negate (deltaX + deltaY)
  return $ HexCoord (x + deltaX, z + deltaZ)

{- Determines the distance between two hexes.  -}
distance :: HexCoord -> HexCoord -> Int
distance (HexCoord (x1, z1)) (HexCoord (x2, z2)) =
  (abs(x1 - x2) + abs(z1 - z2) +
    abs(x1 + z1 - x2 - z2)) `div` 2

{- Determine the position of the center of a hex.
   The distance between hexes is considered to be
   1.0.  The hex at (HexCoord (0,0)) is centered
   on (0.0,0.0).

   This assumes a typical "computer" coordinate
   system, where screen X increases to the right
   and screen Y increases downwards.  -}
position :: Fractional n => HexCoord -> (n, n)
position (HexCoord (xi, zi)) = (xp, yp)
 where
  x = fromIntegral xi
  z = fromIntegral zi
  xp = (x * 0.75) / half_sqrt3
  yp = (x / 2) + z
half_sqrt3 :: Fractional n => n
half_sqrt3 = (1.732050807568877293527446341505 / 2)

{- Approximates the hex coordinates from a given
   position.  -}
fromPosition :: RealFrac n => (n,n) -> HexCoord
fromPosition (xp,yp) = HexCoord (xi,zi)
 where
  x = xp * half_sqrt3 * 4 / 3
  z = yp - (x / 2)
  y = negate $ x + z

  rx = round x :: Int
  ry = round y :: Int
  rz = round z :: Int

  x_diff = abs $ fromIntegral rx - x
  y_diff = abs $ fromIntegral ry - y
  z_diff = abs $ fromIntegral rz - z

  (xi,zi)
    | x_diff > y_diff && x_diff > z_diff = (negate (ry + rz), rz)
    | y_diff > z_diff                    = (rx, rz)
    | otherwise                          = (rx, negate (rx + ry))
