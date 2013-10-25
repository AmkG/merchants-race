{- Core data structures, not for public use!  -}

module Math.Geom.QuadEdge.Data
  ( QEP(..)
  , QE(..)
  , QEtb(..)
  , Index(..)
  , Direction(..)
  , incrDir
  , decrDir
  , Orientation(..)
  , EdgeRef(..)
  , EdgeTable(..)
  , fromStorage
  , toStorage
  ) where

import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.STRef
import Data.Word

type Index = Word

data QE s a
  = QE
    { qeTable :: STRef s (QEtb s a)
    , qeSize :: STRef s Index
    }
data QEtb s a
  = QEtb
    { qeTable1A :: STUArray s Index Word64
    , qeTable2A :: STUArray s Index Word64
    , qeInfo0A :: STArray s Index a
    , qeInfo1A :: STArray s Index a
    , qeInfo2A :: STArray s Index a
    , qeInfo3A :: STArray s Index a
    }

data QEP a
  = QEP
    { qepTable1A :: UArray Index Word64
    , qepTable2A :: UArray Index Word64
    , qepInfo0A :: Array Index a
    , qepInfo1A :: Array Index a
    , qepInfo2A :: Array Index a
    , qepInfo3A :: Array Index a
    , qepSize :: Index
    }

data Direction
  = Rot0
  | Rot1
  | Rot2
  | Rot3
  deriving (Show, Read, Eq, Ord)
incrDir :: Direction -> Direction
incrDir Rot0 = Rot1
incrDir Rot1 = Rot2
incrDir Rot2 = Rot3
incrDir Rot3 = Rot0
decrDir :: Direction -> Direction
decrDir Rot1 = Rot0
decrDir Rot2 = Rot1
decrDir Rot3 = Rot2
decrDir Rot0 = Rot3

data Orientation
  = Normal
  | Flipped
  deriving (Show, Read, Eq, Ord)

type EdgeRef = (Index, Direction, Orientation)
type EdgeTable = (EdgeRef, EdgeRef, EdgeRef, EdgeRef)

fromStorage :: Word64 -> (EdgeRef, EdgeRef)
fromStorage w =
  let wa = (w `shiftR` 32) .&. mask32
      wb = w .&. mask32
  in (fromStorage1 wa, fromStorage1 wb)
 where
  fromStorage1 w =
    let index = fromIntegral $ w `shiftR` 3
        idir = (w `shiftR` 1) .&. 3
        iorient = w .&. 1
        dir = case idir of
          0 -> Rot0
          1 -> Rot1
          2 -> Rot2
          3 -> Rot3
        orient = case iorient of
          0 -> Normal
          1 -> Flipped
    in (index, dir, orient)
toStorage :: (EdgeRef, EdgeRef) -> Word64
toStorage (a, b) =
  let wa = toStorage1 a
      wb = toStorage1 b
  in (wa `shiftL` 32) .|. wb
 where
  toStorage1 (index, dir, orient) =
    let idir = case dir of
          Rot0 -> 0
          Rot1 -> 1
          Rot2 -> 2
          Rot3 -> 3
        iorient = case orient of
          Normal  -> 0
          Flipped -> 1
    in (fromIntegral index `shiftL` 3) .|. (idir `shiftL` 1) .|. iorient

mask32 :: Word64
mask32 = (1 `shiftL` 32) - 1

