{- Math.Geom.Delaunay - Delaunay algorithm implementation.

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
{- Delaunay divide-and-conquer algorithm, from
   Guibas and Stolfi.  -}

{- NOTE: Will FAIL if using Double!  Use Rational instead;
   inexact math (Double) does not give enough bits of
   precision.  Supposedly, alternating the partition between
   horizontal and vertical will make it far more likely to
   succeed using inexact math, but that's beyond my
   capability.  -}

module Math.Geom.Delaunay
  ( delaunay
  ) where

import Math.Geom.QuadEdge

import Control.Monad
import Control.Monad.ST
import Data.List

connect :: QE s (r,r) -> EdgeRef -> EdgeRef -> ST s EdgeRef
connect qeds a b = do
  e <- makeEdge qeds
  putData qeds e       =<< getData qeds (sym a)
  putData qeds (sym e) =<< getData qeds b

  lnext_a <- lnext qeds a
  splice qeds e lnext_a

  splice qeds (sym e) b

  return e

delaunay :: (Ord r, Num r) => [(r,r)] -> ST s (QE s (r,r), EdgeRef, EdgeRef)
delaunay points = do
  let sortedPoints = nub $ sortBy xycompare points
  qeds <- newQE
  (ledge, redge) <- delaunay1 qeds sortedPoints
  return (qeds, ledge, redge)
 where
  delaunay1 qeds = loop
   where
    loop []      = fail "Math.Geom.Delaunay.delaunay: pointless graph."
    loop [a]     = fail "Math.Geom.Delaunay.delaunay: graph has one point."
    -- two-point case
    loop [a,b]   = do
      e <- makeEdge qeds
      putData qeds e a
      putData qeds (sym e) b
      return (e, sym e)
    -- three-point case
    loop [a,b,c] = do
      -- Build the first two edges:
      {-   . b
       e1 / \ e2
         /   \
      a .     . c
      -}
      e1 <- makeEdge qeds
      e2 <- makeEdge qeds
      splice qeds (sym e1) e2
      putData qeds e1 a
      putData qeds (sym e1) b
      putData qeds e2 b
      putData qeds (sym e2) c
      -- Close the triangle
      let action
            | ccw a b c = do
                e3 <- connect qeds e2 e1
                return (e1, sym e2)
            | ccw a c b = do
                e3 <- connect qeds e2 e1
                return (sym e3, e3)
            | otherwise = do
                -- collinear
                return (e1, sym e2)
      action
    -- recursive case
    loop s       = do
      let getOrgDst e = do
            org <- getData qeds e
            dst <- getData qeds (sym e)
            return (org, dst)
      -- Let l and r be the left and right halves of s.
      let (l, r) = splitList s
      (ldo, ldi) <- loop l; (rdi, rdo) <- loop r
      -- compute the lower common tangent of l and r:
      let lowerTangent ldi rdi = do
            (org_rdi, dst_rdi) <- getOrgDst rdi
            (org_ldi, dst_ldi) <- getOrgDst ldi
            let action
                  | ccw org_rdi org_ldi dst_ldi  = do
                      lnext_ldi <- lnext qeds ldi
                      lowerTangent lnext_ldi rdi
                  | ccw org_ldi dst_rdi org_rdi  = do
                      rprev_rdi <- rprev qeds rdi
                      lowerTangent ldi       rprev_rdi
                  | otherwise                    = do
                      return (ldi, rdi)
            action
      (ldi, rdi) <- lowerTangent ldi rdi
      -- create a first cross edge base1 from rdi.Org to ldi.Org
      base1 <- connect qeds (sym rdi) ldi
      (ldo, rdo) <- do
        (org_rdi, _) <- getOrgDst rdi
        (org_ldi, _) <- getOrgDst ldi
        (org_rdo, _) <- getOrgDst rdo
        (org_ldo, _) <- getOrgDst ldo
        return ( if org_ldi == org_ldo then sym base1 else ldo
               , if org_rdi == org_rdo then base1 else rdo
               )
      let merge base1 = do -- This is the merge loop
            (org_base1, dst_base1) <- getOrgDst base1
            let valid e = do
                  dst_e <- getData qeds (sym e)
                  return $ ccw dst_e dst_base1 org_base1
            -- Locate the first L point (lcand.Dest) to be
            -- encountered by the rising bubble, and delete
            -- L edges out of base1.dest that fail the
            -- circle test.
            initial_lcand <- onext qeds (sym base1)
            valid_initial_lcand <- valid initial_lcand
            lcand <- if (not valid_initial_lcand)
              then return initial_lcand
              else do
                let while lcand = do
                      onext_lcand <- onext qeds lcand
                      (_, dst_lcand) <- getOrgDst lcand
                      (_, dst_onext_lcand) <- getOrgDst onext_lcand
                      if inCircle dst_base1 org_base1 dst_lcand dst_onext_lcand
                        then do deleteEdge qeds lcand; while onext_lcand
                        else return lcand
                while initial_lcand
            -- Symmetrically, locate the first R point to
            -- be hit, and delete R edges
            initial_rcand <- oprev qeds base1
            valid_initial_rcand <- valid initial_rcand
            rcand <- if (not valid_initial_rcand)
              then return initial_rcand
              else do
                let while rcand = do
                      oprev_rcand <- oprev qeds rcand
                      (_, dst_rcand) <- getOrgDst rcand
                      (_, dst_oprev_rcand) <- getOrgDst oprev_rcand
                      if inCircle dst_base1 org_base1 dst_rcand dst_oprev_rcand
                        then do deleteEdge qeds rcand; while oprev_rcand
                        else return rcand
                while initial_rcand
            valid_lcand <- valid lcand
            valid_rcand <- valid rcand
            (org_lcand, dst_lcand) <- getOrgDst lcand
            (org_rcand, dst_rcand) <- getOrgDst rcand
            let action
                  -- if both lcand and rcand are invalid, then
                  -- base1 is the upper common tangent.
                  | not valid_lcand && not valid_rcand = return ()
                  -- the next cross edge is to be connected to
                  -- either lcand.Dest or rcand.Dest.
                  -- if both are valid, then choose the
                  -- appropriate one using the inCircle
                  -- test
                  | not valid_lcand ||
                    (valid_rcand && inCircle dst_lcand org_lcand org_rcand dst_rcand)
                                                       = do
                    next_base1 <- connect qeds rcand (sym base1)
                    merge next_base1
                  | otherwise                          = do
                    next_base1 <- connect qeds (sym base1) (sym lcand)
                    merge next_base1
            action
      merge base1
      return (ldo, rdo)

splitList :: [a] -> ([a], [a])
splitList a = loop a a
 where
  -- The first argument is the fast pointer, the
  -- second argument is the slow pointer.
  loop []         slow     = ([], slow)
  loop (_:[])     slow     = ([], slow)
  loop (_:_:fast) (a:slow) = let res = loop fast slow
                             in (a:fst res, snd res)

xycompare :: Ord r => (r, r) -> (r, r) -> Ordering
xycompare (xa, ya) (xb, yb) =
  case compare xa xb of
    EQ  -> compare ya yb
    res -> res

-- Geometric tests
-- Determine if point d is inside the circle
-- defined by points abc.
inCircle :: (Ord r, Num r) => (r, r) -> (r, r) -> (r, r) -> (r, r) -> Bool
inCircle (xa, ya) (xb, yb) (xc, yc) (xd, yd) =
  {- Determine the sign of the determinant of the matrix:
     | xa ya (xa*xa + ya*ya) 1 |
     | xb yb (xb*xb + yb*yb) 1 |
     | xc yc (xc*xc + yc*yc) 1 |
     | xd yd (xd*xd + yd*yd) 1 |
  -}
  let za = xa * xa + ya * ya
      zb = xb * xb + yb * yb
      zc = xc * xc + yc * yc
      zd = xd * xd + yd * yd
      determinant = det4
        (xa, ya, za, 1)
        (xb, yb, zb, 1)
        (xc, yc, zc, 1)
        (xd, yd, zd, 1)
  in determinant > 0
-- Determine if the triangle formed by
-- points abc is clockwise
ccw :: (Ord r, Num r) => (r,r) -> (r,r) -> (r,r) -> Bool
ccw (xa, ya) (xb, yb) (xc, yc) =
  {- Determine the sign of the determinant of the matrix:
    | xa ya 1 |
    | xb yb 1 |
    | xc yc 1 |
  -}
  let determinant = det3
        (xa, ya, 1)
        (xb, yb, 1)
        (xc, yc, 1)
  in determinant > 0

-- determinant computation
det4 :: Num r => (r,r,r,r) -> (r,r,r,r) -> (r,r,r,r) -> (r,r,r,r) -> r
det4 (a0, a1, a2, a3)
     (b0, b1, b2, b3)
     (c0, c1, c2, c3)
     (d0, d1, d2, d3)
  = a0 * det3 (b1, b2, b3)
              (c1, c2, c3)
              (d1, d2, d3)
  - a1 * det3 (b0, b2, b3)
              (c0, c2, c3)
              (d0, d2, d3)
  + a2 * det3 (b0, b1, b3)
              (c0, c1, c3)
              (d0, d1, d3)
  - a3 * det3 (b0, b1, b2)
              (c0, c1, c2)
              (d0, d1, d2)
det3 :: Num r => (r,r,r) -> (r,r,r) -> (r,r,r) -> r
det3 (a0, a1, a2)
     (b0, b1, b2)
     (c0, c1, c2)
  = a0 * det2 (b1, b2)
              (c1, c2)
  - a1 * det2 (b0, b2)
              (c0, c2)
  + a2 * det2 (b0, b1)
              (c0, c1)
det2 :: Num r => (r,r) -> (r,r) -> r
det2 (a0, a1)
     (b0, b1)
  = a0 * b1 - b0 * a1

