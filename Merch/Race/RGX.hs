{- Merch.Race.Control.Background - Background task execution.

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
module Merch.Race.RGX
  ( RGX
  , mkRGX
  , feedRGX
  ) where

import Merch.Race.Data.Serialize

import Data.Bits
import Data.Word
import System.Random

data RGX
  = RGX !Word32 !Word32 !Word32 !Word32
  deriving (Show, Read)

-- Paper "Xorshift RNGs", by Marsaglia, George
get :: RGX -> (Word32, RGX)
get (RGX x y z w) = (rv, RGX y z w rv)
 where
  tmp = x `xor` (x `shiftL` 15)
  rv = (w `xor` (w `shiftR` 21)) `xor` (tmp `xor` (tmp `shiftR` 4))

instance RandomGen RGX where
  genRange _ = (minBound, maxBound)
  next g = let (w, g') = get g in (fromIntegral w, g')
  split g = (RGX x y z w, g3)
   where
    (x, g0) = get g
    (y, g1) = get g0
    (z, g2) = get g1
    (w, g3) = get g2
-- RGX can also be generated from other generators.
instance Random RGX where
  -- range makes no sense for RGX.
  randomR _ = random
  random g = (RGX x y z w, g3)
   where
    (ix, g0) = next g
    (iy, g1) = next g0
    (iz, g2) = next g1
    (iw, g3) = next g2
    x = fromIntegral ix
    y = fromIntegral iy
    z = fromIntegral iz
    w = fromIntegral iw
instance Serialize RGX where
  hPut = hPutConvert $ \ (RGX x y z w) -> (x, y, z, w)
  hGet = hGetConvert $ \ (x, y, z, w)  -> (RGX x y z w)
 
mkRGX :: Int -> RGX
mkRGX i = RGX ri (ri `xor` 1) (ri `xor` 2) (ri `xor` 4)
 where
  ri = fromIntegral i
feedRGX :: Show a => a -> RGX -> RGX
feedRGX a = feed (show a)

feed :: String -> RGX -> RGX
feed []     g = g
feed (c:cs) g = let (_, RGX x y z w) = get g
                in feed cs $ RGX x y z (w `xor` (fromIntegral $ fromEnum c))
