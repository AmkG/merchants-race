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

-}

module Merch.Race.Hex
  ( HexCoord(..)
  , neighbors
  , nearby
  , distance

  , position

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
   adjacent to the given hex.  -}
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
   on (0.0,0.0).  -}
position :: Fractional n => HexCoord -> (n, n)
position (HexCoord (xi, zi)) = (xp, yp)
 where
  x = fromIntegral xi
  z = fromIntegral zi
  xp = (x * 0.75) / half_sqrt3
  yp = (x / 2) + z
half_sqrt3 :: Fractional n => n
half_sqrt3 = (1.732050807568877293527446341505 / 2)

