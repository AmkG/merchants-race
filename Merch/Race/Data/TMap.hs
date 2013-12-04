
{- Terrain Map.  -}
module Merch.Race.Data.TMap
  ( TMap
  , boundsTMap -- TMap -> (HexCoord, HexCoord)
  , lookupTMap -- TMap -> HexCoord -> (Terrain, Bool)
  , terrainsTMap -- TMap -> [(Terrain, Bool)]
  , settlementsTMap -- TMap -> [(Settlement, SettlementType, HexCoord)]
  , settlementAtTMap -- Monad m => TMap -> HexCoord -> m (Settlement, SettlementType)
  , distanceTMap -- TMap -> Settlement -> Settlement -> Distance

  , MTMap
  , newMTMap -- (HexCoord, HexCoord) -> IO MTMap
  , boundsMTMap -- MTMap -> IO (HexCoord, HexCoord)
  , readMTMap -- MTMap -> HexCoord -> IO (Terrain, Bool)
  , writeMTMap -- MTMap -> HexCoord -> (Terrain, Bool) -> IO ()
  , settlementsMTMap -- MTMap -> IO [(Settlement, SettlementType, HexCoord)]
  , settlementAtMTMap -- MTMap -> HexCoord -> IO (Maybe (Settlement, SettlementType))
  , addSettlementMTMap -- MTMap -> Settlement -> SettlementType -> HexCoord -> IO ()
  , readDistanceMTMap -- MTMap -> Settlement -> Settlement -> IO Distance
  , writeDistanceMTMap -- MTMap -> Settlement -> Settlement -> Distance -> IO ()

  , freezeMTMap -- MTMap -> IO TMap
  , unfreezeTMap -- TMap -> IO MTMap
  ) where

import Merch.Race.Data
import Merch.Race.Data.Serialize
import Merch.Race.Hex

import Control.Monad
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
    , distmapTMap :: Map (Settlement, Settlement) Distance
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
terrainsTMap :: TMap -> [(Terrain, Bool)]
{-# INLINE terrainsTMap #-}
terrainsTMap tmap = concatMap translate $ elems $ arrTMap tmap
 where
  {-# INLINE translate #-}
  translate word = [fst tword, snd tword]
   where
    tword = unmarshal word

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
settlementAtTMap tmap h = do
  let v = Map.lookup h $ settlemapTMap tmap
  case v of
    Just v  -> return v
    Nothing -> fail "No settlement."

distanceTMap :: TMap -> Settlement -> Settlement -> Distance
distanceTMap tmap a b
  | a < b     = core (a,b)
  | otherwise = core (b,a)
 where
  core k = fromJust $ Map.lookup k (distmapTMap tmap)

instance Serialize TMap where
  hPut h tm = do
    hPut h $ boundsTMap tm
    let iarr = arrTMap tm
    arr <- thaw iarr
    hPutArray h arr $ rangeSize $ bounds iarr
    hPut h $ settlemapTMap tm
    hPut h $ distmapTMap tm
  hGet h = do
    let tm = TMap { }
    tm <- hGet h >>= \v -> return $ tm { boundsTMap = v }
    let rawI = rangeSize $ boundsTMap tm
        i = (rawI + 1) `div` 2
    arr <- newArray (0, i - 1) 0
    hGetArray h arr i
    tm <- freeze arr >>= \v -> return $ tm { arrTMap = v }
    tm <- hGet h >>= \v -> return $ tm { settlemapTMap = v }
    tm <- hGet h >>= \v -> return $ tm { distmapTMap = v }
    return tm

-------------------------------------------------------------------------------

data MTMap
  = MTMap
    { boundsMTMapP :: (HexCoord, HexCoord)
    , arrMTMap :: IOUArray Int Word8
    , settlemapvarMTMap :: IORef (Map HexCoord (Settlement, SettlementType))
    , distmapvarMTMap :: IORef (Map (Settlement, Settlement) Distance)
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

readMTMap :: MTMap -> HexCoord -> IO (Terrain, Bool)
readMTMap mtmap h = do
  let b = boundsMTMapP mtmap
  if inRange b h
   then do
    let rawI = index b h
        (i, sel) = rawI `divMod` 2
    word <- readArray (arrMTMap mtmap) i
    let tword = unmarshal word
        func
          | sel == 0  = fst
          | otherwise = snd
    return $ func tword
   else do
    return (Sea, False)
writeMTMap :: MTMap -> HexCoord -> (Terrain, Bool) -> IO ()
writeMTMap mtmap h tr = do
  let b = boundsMTMapP mtmap
  when (inRange b h) $ do
    let rawI = index b h
        (i, sel) = rawI `divMod` 2
    word <- readArray (arrMTMap mtmap) i
    let tword = unmarshal word
        unproj
          | sel == 0  =  \ (_, b) a -> (a, b)
          | otherwise =  \ (a, _) b -> (a, b)
        tword' = unproj tword tr
    writeArray (arrMTMap mtmap) i (marshal tword')

settlementsMTMap :: MTMap -> IO [(Settlement, SettlementType, HexCoord)]
settlementsMTMap mtmap = do
  settlemap <- readIORef $ settlemapvarMTMap mtmap
  return $ map (\ (h, (s, st)) -> (s, st, h)) $ Map.toList settlemap

settlementAtMTMap :: MTMap -> HexCoord -> IO (Maybe (Settlement, SettlementType))
settlementAtMTMap mtmap h = do
  settlemap <- readIORef $ settlemapvarMTMap mtmap
  return $ Map.lookup h settlemap

addSettlementMTMap :: MTMap -> Settlement -> SettlementType -> HexCoord -> IO ()
addSettlementMTMap mtmap s st h = do
  modifyIORef (settlemapvarMTMap mtmap) $
    Map.insert h (s,st)

readDistanceMTMap :: MTMap -> Settlement -> Settlement -> IO Distance
readDistanceMTMap mtmap a b
  | a < b     = core (a,b)
  | otherwise = core (b,a)
 where
  core k = readIORef (distmapvarMTMap mtmap)
       >>= return . fromJust . Map.lookup k

writeDistanceMTMap :: MTMap -> Settlement -> Settlement -> Distance -> IO ()
writeDistanceMTMap mtmap a b i
  | a < b     = core (a, b)
  | otherwise = core (b, a)
 where
  core k = do
    modifyIORef (distmapvarMTMap mtmap) $
      Map.insert k i

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
