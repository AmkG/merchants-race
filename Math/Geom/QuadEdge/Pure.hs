{- Quad-edge data structure pure implementation.  -}
module Math.Geom.QuadEdge.Pure
  ( QEP
  , QE
  , EdgeRef(..)
  -- constructions
  , freezeQE -- QE -> ST s ()
  , thawQEP
  -- Queries
  , rot -- EdgeRef -> EdgeRef
  , rotInv -- EdgeRef -> EdgeRef
  , sym -- EdgeRef -> EdgeRef
  , flip -- EdgeRef -> EdgeRef
  , isPrimal -- EdgeRef -> Bool
  , isDual -- EdgeRef -> Bool
  -- Datum access
  , getData -- QEP a -> EdgeRef -> a
  -- connectivity queries
  , oprev -- QEP a -> EdgeRef -> EdgeRef
  , onext -- QEP a -> EdgeRef -> EdgeRef
  , lprev -- QEP a -> EdgeRef -> EdgeRef
  , lnext -- QEP a -> EdgeRef -> EdgeRef
  , rprev -- QEP a -> EdgeRef -> EdgeRef
  , rnext -- QEP a -> EdgeRef -> EdgeRef
  , dprev -- QEP a -> EdgeRef -> EdgeRef
  , dnext -- QEP a -> EdgeRef -> EdgeRef
  ) where

import Prelude hiding (flip)

import Math.Geom.QuadEdge.Data
import Math.Geom.QuadEdge(EdgeRef, rot, rotInv, sym, flip, isPrimal, isDual)

import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray
import Data.STRef

getData :: QEP a -> EdgeRef -> a
getData qep (i, Rot0, _) = qepInfo0A qep ! i
getData qep (i, Rot1, _) = qepInfo1A qep ! i
getData qep (i, Rot2, _) = qepInfo2A qep ! i
getData qep (i, Rot3, _) = qepInfo3A qep ! i

oprev = rot `compose` rot
-- core
onext :: QEP a -> EdgeRef -> EdgeRef
onext qep (i, r, Normal)  = lookupET qep i r
onext qep (i, r, Flipped) = flip $ rot $ lookupET qep i (incrDir r)
lprev = rot `compose` rotInv
lnext = sym `compose` id
rprev = rotInv `compose` rot
rnext = id `compose` sym
dprev = sym `compose` sym
dnext = rotInv `compose` rotInv

compose :: (EdgeRef -> a) -> (b -> EdgeRef) -> QEP d -> b -> a
compose g f qep x = g (onext qep (f x))

lookupET :: QEP a -> Index -> Direction -> EdgeRef
lookupET qep i Rot0 = let (a, _) = fromStorage $ qepTable1A qep ! i in a
lookupET qep i Rot1 = let (_, b) = fromStorage $ qepTable1A qep ! i in b
lookupET qep i Rot2 = let (c, _) = fromStorage $ qepTable2A qep ! i in c
lookupET qep i Rot3 = let (_, d) = fromStorage $ qepTable2A qep ! i in d

thawQEP :: QEP a -> ST s (QE s a)
thawQEP qep = do
  tb1 <- thaw (qepTable1A qep)
  tb2 <- thaw (qepTable2A qep)
  info0 <- thaw (qepInfo0A qep)
  info1 <- thaw (qepInfo1A qep)
  info2 <- thaw (qepInfo2A qep)
  info3 <- thaw (qepInfo3A qep)
  let size = qepSize qep

  tbref <- newSTRef $ QEtb tb1 tb2 info0 info1 info2 info3
  sizeref <- newSTRef $ size

  return $ QE tbref sizeref
  
freezeQE :: QE s a -> ST s (QEP a)
freezeQE qeds = do
  qetb <- readSTRef (qeTable qeds)
  tb1 <- freeze (qeTable1A qetb)
  tb2 <- freeze (qeTable2A qetb)
  info0 <- freeze (qeInfo0A qetb)
  info1 <- freeze (qeInfo1A qetb)
  info2 <- freeze (qeInfo2A qetb)
  info3 <- freeze (qeInfo3A qetb)
  size <- readSTRef (qeSize qeds)

  return $ QEP tb1 tb2 info0 info1 info2 info3 size
