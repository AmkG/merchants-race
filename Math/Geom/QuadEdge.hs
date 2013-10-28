{- Quad-edge data structure implementation, from
   Guibas and Stolfi 1985.  -}

module Math.Geom.QuadEdge
  ( QE
  , EdgeRef(..)
  -- Constructions
  , newQE -- ST s (QE s a)
  , mapQEM -- QE s a -> (EdgeRef -> ST s a -> ST s b) -> ST s (QE s b)
  -- Queries
  , rot -- EdgeRef -> EdgeRef
  , rotInv -- EdgeRef -> EdgeRef
  , sym -- EdgeRef -> EdgeRef
  , flip -- EdgeRef -> EdgeRef
  , isPrimal -- EdgeRef -> Bool
  , isDual -- EdgeRef -> Bool
  -- Datum access
  , getData -- QE s a -> EdgeRef -> ST s a
  , putData -- QE s a -> EdgeRef -> a -> ST s ()
  -- Monadic queries
  , oprev -- QE s a -> EdgeRef -> ST s EdgeRef
  , onext -- QE s a -> EdgeRef -> ST s EdgeRef
  , lprev -- QE s a -> EdgeRef -> ST s EdgeRef
  , lnext -- QE s a -> EdgeRef -> ST s EdgeRef
  , rprev -- QE s a -> EdgeRef -> ST s EdgeRef
  , rnext -- QE s a -> EdgeRef -> ST s EdgeRef
  , dprev -- QE s a -> EdgeRef -> ST s EdgeRef
  , dnext -- QE s a -> EdgeRef -> ST s EdgeRef
  -- Basic topological operations
  , makeEdge -- QE s a -> a -> ST s EdgeRef
  , splice -- QE s a -> EdgeRef -> EdgeRef -> ST s ()
  -- Topological operations for Delaunay/Voronoi diagrams
  , deleteEdge -- QE s a -> EdgeRef -> ST s ()
  ) where

import Prelude hiding (flip)

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Bits
import Data.STRef
import Data.Word

import Math.Geom.QuadEdge.Data

rot :: EdgeRef -> EdgeRef
rot (e, r, Normal)  = (e, incrDir r, Normal)
rot (e, r, Flipped) = (e, decrDir r, Flipped)
rotInv :: EdgeRef -> EdgeRef
rotInv (e, r, Normal)  = (e, decrDir r, Normal)
rotInv (e, r, Flipped) = (e, incrDir r, Flipped)
sym :: EdgeRef -> EdgeRef
sym = rot . rot
flip :: EdgeRef -> EdgeRef
flip (e, r, Normal)  = (e, r, Flipped)
flip (e, r, Flipped) = (e, r, Normal)

getQEtb :: QE s a -> ST s (QEtb s a)
getQEtb qeds = do
  let qetbref = qeTable qeds
  readSTRef qetbref

lookupET :: QE s a -> Index -> Direction -> ST s EdgeRef
lookupET qeds i r = do
  qetb <- getQEtb qeds
  let tb1 = qeTable1A qetb
      tb2 = qeTable2A qetb
  case r of
    Rot0 -> do
        ab <- readArray tb1 i
        let (a, _) = fromStorage ab
        return a
    Rot1 -> do
        ab <- readArray tb1 i
        let (_, b) = fromStorage ab
        return b
    Rot2 -> do
        cd <- readArray tb2 i
        let (c, _) = fromStorage cd
        return c
    Rot3 -> do
        cd <- readArray tb2 i
        let (_, d) = fromStorage cd
        return d

updateET :: QE s a -> Index -> Direction -> EdgeRef -> ST s ()
updateET qeds i r v = do
  qetb <- getQEtb qeds
  let tb1 = qeTable1A qetb
      tb2 = qeTable2A qetb
  case r of
    Rot0 -> do
        ab <- readArray tb1 i
        let (_, b) = fromStorage ab
        writeArray tb1 i $ toStorage (v, b)
    Rot1 -> do
        ab <- readArray tb1 i
        let (a, _) = fromStorage ab
        writeArray tb1 i $ toStorage (a, v)
    Rot2 -> do
        cd <- readArray tb2 i
        let (_, d) = fromStorage cd
        writeArray tb2 i $ toStorage (v, d)
    Rot3 -> do
        cd <- readArray tb2 i
        let (c, _) = fromStorage cd
        writeArray tb2 i $ toStorage (c, v)

newQE :: ST s (QE s a)
newQE = do
  tb1 <- newArray_ (0, 1023)
  tb2 <- newArray_ (0, 1023)
  info0 <- newArray_ (0, 1023)
  info1 <- newArray_ (0, 1023)
  info2 <- newArray_ (0, 1023)
  info3 <- newArray_ (0, 1023)
  qetbref <- newSTRef $ QEtb
    { qeTable1A = tb1
    , qeTable2A = tb2
    , qeInfo0A = info0
    , qeInfo1A = info1
    , qeInfo2A = info2
    , qeInfo3A = info3
    }
  sizeref <- newSTRef 0
  return $ QE
    { qeTable = qetbref
    , qeSize = sizeref
    }

mapQEM :: QE s a -> (EdgeRef -> ST s a -> ST s b) -> ST s (QE s b)
mapQEM qeds mf = do
  let sizeref = qeSize qeds
      qetbref = qeTable qeds
  size <- readSTRef sizeref
  qetb <- readSTRef qetbref

  rv <- do
    (_, bnd) <- getBounds $ qeTable1A qetb
    tb1 <- copyArray $ qeTable1A qetb
    tb2 <- copyArray $ qeTable2A qetb
    info0 <- newArray_ (0, bnd)
    info1 <- newArray_ (0, bnd)
    info2 <- newArray_ (0, bnd)
    info3 <- newArray_ (0, bnd)
    qetbref <- newSTRef $ QEtb
      { qeTable1A = tb1
      , qeTable2A = tb2
      , qeInfo0A = info0
      , qeInfo1A = info1
      , qeInfo2A = info2
      , qeInfo3A = info3
      }
    sizeref <- newSTRef size
    return $ QE
      { qeTable = qetbref
      , qeSize = sizeref
      }
  forM_ [0..size-1] $ \i -> do
    let refs = map (\r -> (i, r, Normal)) [Rot0, Rot1, Rot2, Rot3]
    forM_ refs $ \e -> do
      b <- mf e (getData qeds e)
      putData rv e b
    return ()
  return rv
 where
  copyArray a = do
    (s, e) <- getBounds a
    rv <- newArray_ (s, e)
    forM_ [s..e] $ \i -> do
      readArray a i >>= writeArray rv i
    return rv

makeEdge :: QE s a -> ST s EdgeRef
makeEdge qeds = do
  expand qeds
  let sizeref = qeSize qeds
  i <- readSTRef sizeref
  writeSTRef sizeref (i + 1)
  let qetbref = qeTable qeds
  qetb <- readSTRef qetbref
  let tb1 = qeTable1A qetb
      tb2 = qeTable2A qetb
      (a, b, c, d) = emptyET i
  writeArray tb1 i $ toStorage (a, b)
  writeArray tb2 i $ toStorage (c, d)
  return a

-- Create a new empty edge table
emptyET :: Index -> EdgeTable
emptyET i = ( (i, Rot0, Normal)
            , (i, Rot3, Normal)
            , (i, Rot2, Normal)
            , (i, Rot1, Normal)
            )

-- Checks if there is space in the data structure,
-- and expands the data structure if no more space.
expand :: QE s a -> ST s ()
expand qeds = do
  let sizeref = qeSize qeds
  size <- readSTRef sizeref
  let qetbref = qeTable qeds
  qetb <- readSTRef qetbref
  let tb1 = qeTable1A qetb
  (_, n) <- getBounds tb1
  when (size == n + 1) $ do
    -- need to resize
    -- make new tables
    let cap = n + 1
        ncap = cap + (cap `div` 2)
        nn = ncap - 1
    ntb1 <- newArray_ (0, nn)
    ntb2 <- newArray_ (0, nn)
    ninfo0 <- newArray_ (0, nn)
    ninfo1 <- newArray_ (0, nn)
    ninfo2 <- newArray_ (0, nn)
    ninfo3 <- newArray_ (0, nn)
    -- copy
    let tb2 = qeTable2A qetb
        info0 = qeInfo0A qetb
        info1 = qeInfo1A qetb
        info2 = qeInfo2A qetb
        info3 = qeInfo3A qetb
    forM_ [0..n] $ \i -> do
      readArray tb1 i >>= writeArray ntb1 i
      readArray tb2 i >>= writeArray ntb2 i
      readArray info0 i >>= writeArray ninfo0 i
      readArray info1 i >>= writeArray ninfo1 i
      readArray info2 i >>= writeArray ninfo2 i
      readArray info3 i >>= writeArray ninfo3 i
    -- update wrapper
    writeSTRef qetbref $ QEtb
      { qeTable1A = ntb1
      , qeTable2A = ntb2
      , qeInfo0A = ninfo0
      , qeInfo1A = ninfo1
      , qeInfo2A = ninfo2
      , qeInfo3A = ninfo3
      }

isPrimal :: EdgeRef -> Bool
isPrimal (_, Rot0, _) = True
isPrimal (_, Rot2, _) = True
isPrimal (_, _,    _) = False
isDual :: EdgeRef -> Bool
isDual (_, Rot1, _) = True
isDual (_, Rot3, _) = True
isDual (_, _,    _) = False

-- Datum access
getData :: QE s a -> EdgeRef -> ST s a
getData qeds (i, r, _) = do
  qetb <- getQEtb qeds
  let info = case r of
               Rot0 -> qeInfo0A qetb
               Rot1 -> qeInfo1A qetb
               Rot2 -> qeInfo2A qetb
               Rot3 -> qeInfo3A qetb
  readArray info i
putData :: QE s a -> EdgeRef -> a -> ST s ()
putData qeds (i, r, _) a = do
  qetb <- getQEtb qeds
  let info = case r of
               Rot0 -> qeInfo0A qetb
               Rot1 -> qeInfo1A qetb
               Rot2 -> qeInfo2A qetb
               Rot3 -> qeInfo3A qetb
  writeArray info i a

-- monadic queries

oprev :: QE s a -> EdgeRef -> ST s EdgeRef
oprev = rot `compose` rot
-- Most basic monadic query
onext :: QE s a -> EdgeRef -> ST s EdgeRef
onext qeds (i, r, Normal)  = lookupET qeds i r
onext qeds (i, r, Flipped) = do
  ref <- lookupET qeds i (incrDir r)
  return $ flip $ rot ref
compose :: (EdgeRef -> a) -> (b -> EdgeRef) -> QE s d -> b -> ST s a
compose g f qeds x = onext qeds (f x) >>= (return . g)

lnext :: QE s a -> EdgeRef -> ST s EdgeRef
lnext = rot `compose` rotInv
lprev :: QE s a -> EdgeRef -> ST s EdgeRef
lprev = sym `compose` id
rnext :: QE s a -> EdgeRef -> ST s EdgeRef
rnext = rotInv `compose` rot
rprev :: QE s a -> EdgeRef -> ST s EdgeRef
rprev = id `compose` sym
dnext :: QE s a -> EdgeRef -> ST s EdgeRef
dnext = sym `compose` sym
dprev :: QE s a -> EdgeRef -> ST s EdgeRef
dprev = rotInv `compose` rotInv

-- basic splice operation
splice :: QE s a -> EdgeRef -> EdgeRef -> ST s ()
splice q a b = do
  when ((isPrimal a && isDual b) || (isDual a && isPrimal b)) $ do
    fail "Math.Geom.QuadEdge.splice: attempt to splice a dual to a primal edge"
  oa <- onext q a
  if b == flip oa
   then return ()
   else do
     ob <- onext q b
     let x = rot oa
         y = rot ob
     ox <- onext q x
     oy <- onext q y
     update a y ob
     update b x oa
     update x b oy
     update y a ox
 where
  update (i, r, Normal)  _ n = updateET q i r           n
  update (i, r, Flipped) z _ = updateET q i (incrDir r) (flip z)

-- Topological operation for Delaunay and Voronoi
deleteEdge :: QE s a -> EdgeRef -> ST s ()
deleteEdge qeds e = do
  oprev_e <- oprev qeds e
  splice qeds e oprev_e

  let sym_e = sym e
  oprev_sym_e <- oprev qeds sym_e
  splice qeds sym_e oprev_sym_e
