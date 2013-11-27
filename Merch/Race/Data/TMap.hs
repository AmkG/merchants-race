
{- Terrain Map.  -}
module Merch.Race.Data.TMap
  ( TMap
  , boundsTMap -- TMap -> (HexCoord, HexCoord)
  , lookupTMap -- TMap -> HexCoord -> (Terrain, Bool)
  , settlementsTMap -- TMap -> [(Settlement, SettlementType, HexCoord)]
  , settlementAtTMap -- Monad m => TMap -> HexCoord -> m (Settlement, SettlementType)
  , distanceTMap -- TMap -> Settlement -> Settlement -> Int

  , MTMap
  , boundsMTMap -- MTMap -> IO (HexCoord, HexCoord)
  , newMTMap -- (HexCoord, HexCoord) -> IO MTMap
{-
  , readMTMap -- MTMap -> HexCoord -> IO (Terrain, Bool)
  , writeMTMap -- MTMap -> HexCoord -> (Terrain, Bool) -> IO ()
  , settlementsMTMap -- MTMap -> IO [(Settlement, SettlementType, HexCoord)]
  , settlementAtMTMap -- MTMap -> HexCoord -> IO (Maybe (Settlement, SettlementType))
  , addSettlementMTMap -- MTMap -> Settlement -> SettlementType -> HexCoord -> IO ()
  , readDistanceMTMap -- MTMap -> Settlement -> Settlement -> IO Int
  , writeDistanceMTMap -- MTMap -> Settlement -> Settlement -> Int -> IO ()

  , hPutMTMap -- Handle -> MTMap -> IO ()
  , hGetMTMap -- Handle -> IO MTMap

-}
  , freezeMTMap -- MTMap -> IO TMap
  , unfreezeTMap -- TMap -> IO MTMap
  ) where

import Merch.Race.Data
import Merch.Race.Hex

import Data.Array.Unboxed
import Data.Array.IO
import Data.Bits
import Data.IORef
import Data.Ix
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Word

data TMap
  = TMap
    { boundsTMap :: (HexCoord, HexCoord)
    , arrTMap :: UArray Int Word8
    , settlemapTMap :: Map HexCoord (Settlement, SettlementType)
    , distmapTMap :: Map (Settlement, Settlement) Int
    }
lookupTMap :: TMap -> HexCoord -> (Terrain, Bool)
lookupTMap tmap h = final
 where
  b = boundsTMap tmap
  rawI = index b h
  (i, sel) = rawI `divMod` 2
  word = arrTMap tmap ! i
  tword = unmarshal word
  func
    | sel == 0  = fst
    | otherwise = snd
  final
    | inRange b h = func tword
    | otherwise   = (Sea, False)

marshal :: ((Terrain, Bool), (Terrain, Bool)) -> Word8
marshal ((tl, rl), (tr, rr)) = word
 where
  marshalT Sea        = 0
  marshalT Freshwater = 2
  marshalT Coast      = 4
  marshalT Plains     = 6
  marshalT Forest     = 8
  marshalT Hill       = 10
  marshalT Mountain   = 12
  marshalR False = 0
  marshalR True  = 1

  wordl = marshalT tl .|. marshalR rl
  wordr = marshalT tr .|. marshalR rr

  word = (wordl `shiftL` 4) .|. wordr
unmarshal :: Word8 -> ((Terrain, Bool), (Terrain, Bool))
unmarshal word = ((tl, rl), (tr, rr))
 where
  unmarshalT  0 = Sea
  unmarshalT  2 = Freshwater
  unmarshalT  4 = Coast
  unmarshalT  6 = Plains
  unmarshalT  8 = Forest
  unmarshalT 10 = Hill
  unmarshalT 12 = Mountain
  unmarshalR 0 = False
  unmarshalR 1 = False
  wordl = (word `shiftR` 4) .&. 15
  wordr = (word `shiftR` 0) .&. 15
  tl = unmarshalT $ wordl .&. 14
  rl = unmarshalR $ wordl .&.  1
  tr = unmarshalT $ wordr .&. 14
  rr = unmarshalR $ wordr .&.  1

settlementsTMap :: TMap -> [(Settlement, SettlementType, HexCoord)]
settlementsTMap tmap = map reformat $ Map.toList $ settlemapTMap tmap
 where
  reformat (h, (set, setT)) = (set, setT, h)

settlementAtTMap :: Monad m =>
                    TMap -> HexCoord -> m (Settlement, SettlementType)
settlementAtTMap tmap h = Map.lookup h $ settlemapTMap tmap

distanceTMap :: TMap -> Settlement -> Settlement -> Int
distanceTMap tmap a b
  | a < b     = core (a,b)
  | otherwise = core (b,a)
 where
  core k = fromJust $ Map.lookup k (distmapTMap tmap)

-------------------------------------------------------------------------------

data MTMap
  = MTMap
    { boundsMTMapP :: (HexCoord, HexCoord)
    , arrMTMap :: IOUArray Int Word8
    , settlemapvarMTMap :: IORef (Map HexCoord (Settlement, SettlementType))
    , distmapvarMTMap :: IORef (Map (Settlement, Settlement) Int)
    }

newMTMap :: (HexCoord, HexCoord) -> IO MTMap
newMTMap b = do
  let rawI = rangeSize b
      i = (rawI + 1) `div` 2
  arr <- newArray (0, i - 1) 0
  settlemapvar <- newIORef Map.empty
  distmapvar <- newIORef Map.empty
  return $ MTMap
           { boundsMTMapP = b
           , arrMTMap = arr
           , settlemapvarMTMap = settlemapvar
           , distmapvarMTMap = distmapvar
           }

boundsMTMap :: MTMap -> IO (HexCoord, HexCoord)
boundsMTMap = return . boundsMTMapP

freezeMTMap :: MTMap -> IO TMap
freezeMTMap mtmap = do
  let b = boundsMTMapP mtmap
  arr <- freeze $ arrMTMap mtmap
  settlemap <- readIORef $ settlemapvarMTMap mtmap
  distmap <- readIORef $ distmapvarMTMap mtmap
  return $ TMap
           { boundsTMap = b
           , arrTMap = arr
           , settlemapTMap = settlemap
           , distmapTMap = distmap
           }

unfreezeTMap :: TMap -> IO MTMap
unfreezeTMap tmap = do
  let b = boundsTMap tmap
  arr <- thaw $ arrTMap tmap
  settlemapvar <- newIORef $ settlemapTMap tmap
  distmapvar <- newIORef $ distmapTMap tmap
  return $ MTMap
           { boundsMTMapP = b
           , arrMTMap = arr
           , settlemapvarMTMap = settlemapvar
           , distmapvarMTMap = distmapvar
           }
