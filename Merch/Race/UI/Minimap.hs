
{- Renders a minimap for the given TMap.  -}
module Merch.Race.UI.Minimap
  ( minimap
  ) where

import Merch.Race.Data
import Merch.Race.Data.TMap
import qualified Merch.Race.Hex as H
import Merch.Race.Hex hiding(position)
import Merch.Race.UI.DrawingCombinators

import Control.Monad
import Data.Ix
import Data.Monoid
import qualified Data.Set as Set
import Data.Set(Set)

{- The minimap is scaled for -1,-1 to 1,1.
   Also, the minimap renders hex coordinates
   positive right and downward, and outputs
   OpenGL coordinates that go positive
   right and upward.

   The parameter hidden is the set of hidden
   tiles (i.e. tiles not yet explored by the
   merchant).  -}
minimap :: TMap -> Set HexCoord -> Image (First HexCoord)
minimap tm hidden = adjustment %% total
 where
  (lb, ub) = boundsTMap tm
  superlb = fromOffset $ (\ (q,r) -> (q-1,r-1)) $ toOffset lb
  superub = fromOffset $ (\ (q,r) -> (q+1,r+1)) $ toOffset ub
  (lowx, lowy)   = position superlb
  (highx, highy) = position superub
  adjustx = 2 / (highx - lowy)
  adjusty = 2 / (highy - lowy)
  supercenter = (negate $ (lowx + highx) / 2, negate $ (lowy + highy) / 2)
  adjustment = scale adjustx adjusty `mappend` translate supercenter

  hs = filter (not . flip Set.member hidden) $ range (lb, ub)

  total = mconcat
          [ forceSample (First Nothing) settlements
          , forceSample (First Nothing) roads
          , tiles
          , forceSample (First Nothing) background
          ]
  background = tint backgroundColor $ rectangle (lowx, lowy) (highx, highy)
  backgroundColor = Color 0 0 0 1

  settlements = mconcat $ map makeSett hs
  makeSett :: HexCoord -> Image Any
  makeSett h
    | not s     = mempty
    | otherwise = tint settlementColor movedcircle
   where
    s = case settlementAtTMap tm h of
          Nothing -> False
          _       -> True
    adjust = translate (position h) `mappend` scale 0.3 0.3
    movedcircle = adjust %% circle
  settlementColor = Color 0.25 0.25 1.0  1

  roads = mconcat $ map makeRoad hs
  makeRoad :: HexCoord -> Image Any
  makeRoad h
    | not r     = mempty
    | otherwise = tint roadColor $ mconcat roadLines
   where
    (_, r) = lookupTMap tm h
    c0 = position h
    roadLines = do
      n <- neighbors h
      when (not $ snd $ lookupTMap tm n) $ do
        fail "no road"
      return $ line c0 (halfway c0 $ position n)
  roadColor = Color 0.25 0.25 0   1

  tiles = mconcat $ map makeHex hs
  makeHex :: HexCoord -> Image (First HexCoord)
  makeHex h = fmap toH $ tint color $ convexPoly coords
   where
    (t, _) = lookupTMap tm h
    color = terrainColor t
    c0 = position h
    [c1, c2, c3, c4, c5, c6] = map position $ neighbors h
    coords = [ center c0 c1 c2
             , center c0 c2 c3
             , center c0 c3 c4
             , center c0 c4 c5
             , center c0 c5 c6
             , center c0 c6 c1
             ]

    toH (Any True)  = First $ Just h
    toH (Any False) = First $ Nothing

  terrainColor Sea        = Color 0.1 0.1 0.9  1
  terrainColor Freshwater = Color 0.3 0.4 1.0  1
  terrainColor Coast      = Color 1.0 0.9 0.3  1
  terrainColor Plains     = Color 0.3 0.9 0.3  1
  terrainColor Forest     = Color 0.0 0.8 0.0  1
  terrainColor Hill       = Color 0.4 0.4 0.1  1
  terrainColor Mountain   = Color 0.6 0.6 0.6  1

-- Get the center of three points
center :: Fractional r => (r,r) -> (r,r) -> (r,r) -> (r,r)
center (x1, y1) (x2, y2) (x3, y3) =
  ((x1 + x2 + x3) / 3, (y1 + y2 + y3) / 3)

-- Get the center of two points
halfway :: Fractional r => (r,r) -> (r,r) -> (r,r)
halfway (x1, y1) (x2, y2) =
  ((x1 + x2) / 2, (y1 + y2) / 2)

-- Our adjusted position, to handle our Hex coordinates
-- having the opposite y direction to the OpenGL coordinates.
-- We just use the expedient of negating the Y direction.
position :: HexCoord -> (R,R)
position h = let (x,y) = H.position h in (x, negate y)
