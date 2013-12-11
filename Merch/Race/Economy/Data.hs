{- Merch.Race.Economy.Data - Data structure for economics.

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

module Merch.Race.Economy.Data
  ( Economy(..)

  , R30
  , newR30
  , pushR30
  , listR30

  , R365
  , newR365
  , pushR365
  , listR365
  ) where

import Merch.Race.Data
import Merch.Race.Data.Serialize
import Merch.Race.RGX

import Control.Monad
import Data.Array
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Word

{- Economic information.  -}
data Economy
  = E
    { ePrice :: Map (Settlement,Item) Price
    , eSurplus :: Map (Settlement,Item) Rational
    , ePreviousError :: Map (Settlement,Item) Rational
    , eIntegral :: Map (Settlement,Item) Rational
    , eToday :: Day

    -- Core RGX generators.  eScheduledRGX is stable
    -- for the whole year, while eProbabilityRGX is
    -- modified each day.
    , eScheduledRGX :: RGX
    , eProbabilityRGX :: RGX

    , eAvePrice :: Map Item Price

    , ePriceRecord :: Map (Settlement,Item) R30
    , eAvePriceRecord :: Map Item R365
    }
  deriving (Show,Read)

{- On-disk economic information.

   This is a separate data type so that backwards compatibility
   is "seamless".  Basically, the Economy data structure can
   change, provided that we are able to provide a conversion
   function from earlier Ev versions to the latest Ev version.  -}
data Ev
  = Ev1 (Map (Settlement, Item) (Price, Rational, Rational, Rational, R30))
        (Map Item (Price, R365))
        Day
        RGX
        RGX
  deriving (Show, Read)
-- Convert Ev to the latest version [toUpdate]
toLatestEv :: Ev -> Ev
toLatestEv (Ev1 m1 m2 d g1 g2) = (Ev1 m1 m2 d g1 g2)
-- Convert Economy to latest Ev version [toUpdate]
toEv :: Economy -> Ev
toEv e = Ev1 m1 m2 d g1 g2
 where
  m1 = Map.mapWithKey expandM1 $ ePrice e
  expandM1 k p =
    ( p
    , maybe 0 id $ Map.lookup k $ eSurplus e
    , maybe 0 id $ Map.lookup k $ ePreviousError e
    , maybe 0 id $ Map.lookup k $ eIntegral e
    , maybe (newR30 p) id $ Map.lookup k $ ePriceRecord e
    )
  m2 = Map.mapWithKey expandM2 $ eAvePrice e
  expandM2 k p =
    ( p
    , maybe (newR365 p) id $ Map.lookup k $ eAvePriceRecord e
    )
  d = eToday e
  g1 = eScheduledRGX e
  g2 = eProbabilityRGX e
-- Convert Ev to Economy
toEconomy :: Ev -> Economy
toEconomy ev = core $ toLatestEv ev
 where
  -- convert latest Ev to Economy [toUpdate]
  core (Ev1 m1 m2 d g1 g2) =
    E
     { ePrice         = Map.map (\ (p,_,_,_,_) -> p) m1
     , eSurplus       = Map.map (\ (_,s,_,_,_) -> s) m1
     , ePreviousError = Map.map (\ (_,_,e,_,_) -> e) m1
     , eIntegral      = Map.map (\ (_,_,_,i,_) -> i) m1
     , ePriceRecord   = Map.map (\ (_,_,_,_,r) -> r) m1

     , eAvePrice       = Map.map (\ (p,_) -> p) m2
     , eAvePriceRecord = Map.map (\ (_,r) -> r) m2

     , eToday          = d
     , eScheduledRGX   = g1
     , eProbabilityRGX = g2
     }
instance Serialize Ev where
  -- Only need to serialize the latest version. [toUpdate]
  hPut h (Ev1 m1 m2 d g1 g2) = do
    hPut h (1 :: Word8)
    hPut h (m1,m2,d,g1,g2)
  -- Need to handle all versions. [toUpdate]
  hGet h = do
    v <- hGet h :: IO Word8
    case v of
      1 -> do
        (m1,m2,d,g1,g2) <- hGet h
        return $ Ev1 m1 m2 d g1 g2

-- Actual serialization of Economy
instance Serialize Economy where
  hPut = hPutConvert toEv
  hGet = hGetConvert toEconomy

{- 30-day record of recent prices.  -}
data R30
  = R30
      Price Price Price Price Price Price
      Price Price Price Price Price Price
      Price Price Price Price Price Price
      Price Price Price Price Price Price
      Price Price Price Price Price Price
  deriving (Show,Read)
newR30 :: Price -> R30
newR30 price
  = price `seq` R30
      price price price price price price
      price price price price price price
      price price price price price price
      price price price price price price
      price price price price price price
pushR30 :: Price -> R30 -> R30
pushR30 p30
  (R30
    p0  p1  p2  p3  p4  p5
    p6  p7  p8  p9  p10 p11
    p12 p13 p14 p15 p16 p17
    p18 p19 p20 p21 p22 p23
    p24 p25 p26 p27 p28 p29)
  = p30 `seq`
  (R30
    p1  p2  p3  p4  p5  p6
    p7  p8  p9  p10 p11 p12
    p13 p14 p15 p16 p17 p18
    p19 p20 p21 p22 p23 p24
    p25 p26 p27 p28 p29 p30)
listR30 :: R30 -> [Price]
listR30
  (R30
    p0  p1  p2  p3  p4  p5
    p6  p7  p8  p9  p10 p11
    p12 p13 p14 p15 p16 p17
    p18 p19 p20 p21 p22 p23
    p24 p25 p26 p27 p28 p29)
  =
  [ p0 , p1 , p2 , p3 , p4 , p5
  , p6 , p7 , p8 , p9 , p10, p11
  , p12, p13, p14, p15, p16, p17
  , p18, p19, p20, p21, p22, p23
  , p24, p25, p26, p27, p28, p29
  ]
instance Serialize R30 where
  hPut h
    (R30
      p0  p1  p2  p3  p4  p5
      p6  p7  p8  p9  p10 p11
      p12 p13 p14 p15 p16 p17
      p18 p19 p20 p21 p22 p23
      p24 p25 p26 p27 p28 p29)
    = do
    hPut h p0;  hPut h p1;  hPut h p2;  hPut h p3;  hPut h p4;  hPut h p5;
    hPut h p6;  hPut h p7;  hPut h p8;  hPut h p9;  hPut h p10; hPut h p11;
    hPut h p12; hPut h p13; hPut h p14; hPut h p15; hPut h p16; hPut h p17;
    hPut h p18; hPut h p19; hPut h p20; hPut h p21; hPut h p22; hPut h p23;
    hPut h p24; hPut h p25; hPut h p26; hPut h p27; hPut h p28; hPut h p29;
  hGet h = do
    p0 <-hGet h;p1 <-hGet h;p2 <-hGet h; p3<-hGet h;p4 <-hGet h;p5 <-hGet h;
    p6 <-hGet h;p7 <-hGet h;p8 <-hGet h; p9<-hGet h;p10<-hGet h;p11<-hGet h;
    p12<-hGet h;p13<-hGet h;p14<-hGet h;p15<-hGet h;p16<-hGet h;p17<-hGet h;
    p18<-hGet h;p19<-hGet h;p20<-hGet h;p21<-hGet h;p22<-hGet h;p23<-hGet h;
    p24<-hGet h;p25<-hGet h;p26<-hGet h;p27<-hGet h;p28<-hGet h;p29<-hGet h;
    return (R30
      p0  p1  p2  p3  p4  p5
      p6  p7  p8  p9  p10 p11
      p12 p13 p14 p15 p16 p17
      p18 p19 p20 p21 p22 p23
      p24 p25 p26 p27 p28 p29)

{- 365-day record of prices.  -}
newtype R365
  = R365 (Array Int Price)
  deriving(Show, Read)
newR365 :: Price -> R365
newR365 price =
  R365 $ listArray (0, 364) $ take 365 $ repeat price
pushR365 :: Price -> R365 -> R365
pushR365 pnew (R365 a) =
  R365 $ listArray (0, 364) $ tail $ elems a ++ [pnew]
listR365 :: R365 -> [Price]
listR365 (R365 a) = elems a
instance Serialize R365 where
  hPut h (R365 a) = do
    forM_ [0..364] $ \i -> do
      hPut h $ a ! i
  hGet h = do
    ps <- forM [0..364] $ \_ -> do
      hGet h
    return $ R365 $ listArray (0, 364) $ ps
