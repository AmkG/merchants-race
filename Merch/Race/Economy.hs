{- Merch.Race.Economy - Wrapper module for economy.

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

module Merch.Race.Economy
  ( Economy
  -- Construction
  , newEconomy
  -- Daily update
  , stepEconomy
  -- For use after loading
  , adaptEconomyToGame

  -- Queries
  , price
  , priceRecord
  , avePrice
  , avePriceRecord
  , available
  , today

  -- Player actions
  , buy
  , sell

  ) where

import Merch.Race.Data
import Merch.Race.Data.TMap
import Merch.Race.Economy.CMerch
import Merch.Race.Economy.Craftsmen
import Merch.Race.Economy.Data
import Merch.Race.Economy.ItemSetPrice
import Merch.Race.Economy.Market
import Merch.Race.Economy.ProdCons
import Merch.Race.RGX
import qualified Merch.Race.Ruleset as RS
import Merch.Race.Ruleset(Ruleset)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Parallel.Strategies
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import System.Random

newEconomy :: RandomGen g
           => (Ruleset, TMap, Difficulty)
           -> (g -> (Economy, g))
newEconomy game g = (adaptEconomyToGame game base, g'')
 where
  (rgx1, g') = random g
  (rgx2, g'') = random g'
  base =
    E
    { ePrice = Map.empty
    , eSurplus = Map.empty
    , ePreviousError = Map.empty
    , eIntegral = Map.empty
    , eToday = 1

    , eScheduledRGX = rgx1
    , eProbabilityRGX = rgx2

    , eAvePrice = Map.empty

    , ePriceRecord = Map.empty
    , eAvePriceRecord = Map.empty
    }

stepEconomy :: RandomGen g
            => (Ruleset, TMap, Difficulty)
            -> Economy
            -> (g -> (Economy, g))
stepEconomy (rs,tm,di) e g = finalE `seq` (finalE, g'')
 where
  (rgx1, g') = random g
  (rgx2, g'') = random g'

  stepE = runEM stepEM (rs,tm,di) e
  aveE = runEM aveEM (rs,tm,di) stepE

  today = eToday aveE
  lastDay = today == 365
  nextDay
    | lastDay   = 1
    | otherwise = today + 1
  finalE = aveE
    { eToday = nextDay
    , eScheduledRGX = if lastDay then rgx2 else eScheduledRGX aveE
    , eProbabilityRGX = rgx1
    , ePriceRecord =
      Map.mapWithKey (\k v -> pushR30 (ePrice aveE Map.! k) v)
                     (ePriceRecord aveE)
    , eAvePriceRecord =
      Map.mapWithKey (\k v -> pushR365 (eAvePrice aveE Map.! k) v)
                     (eAvePriceRecord aveE)
    }

adaptEconomyToGame :: (Ruleset, TMap, Difficulty) -> Economy -> Economy
adaptEconomyToGame (rs,tm,di) e =
  e
  { ePrice = putDefault sis cprice $ ePrice e
  , eSurplus = putDefault sis 0 $ eSurplus e
  , ePreviousError = putDefault sis 0 $ ePreviousError e
  , eIntegral = putDefault sis 0 $ eIntegral e
  , eAvePrice = putDefault items cprice $ eAvePrice e
  , ePriceRecord = putDefault sis (newR30 cprice) $ ePriceRecord e
  , eAvePriceRecord = putDefault items (newR365 cprice) $ eAvePriceRecord e
  }
 where
  putDefault ks v mp = foldl' default1 mp ks
   where
    default1 mp k =
      case Map.lookup k mp of
        Just _  -> mp
        Nothing -> Map.insert k v mp
  settlements = map (\ (s, _, _) -> s) $ settlementsTMap tm
  items = RS.items rs
  sis = [(s,i) | s <- settlements, i <- items]
  cprice = RS.centerPrice rs di

{- Queries.  -}
price :: Settlement -> Item -> Economy -> Price
price s i e = ePrice e Map.! (s, i)
priceRecord :: Settlement -> Item -> Economy -> [Price]
priceRecord s i e = listR30 $ ePriceRecord e Map.! (s, i)
avePrice :: Item -> Economy -> Price
avePrice i e = eAvePrice e Map.! i
avePriceRecord :: Item -> Economy -> [Price]
avePriceRecord i e = listR365 $ eAvePriceRecord e Map.! i
available :: Settlement -> Item -> Economy -> Int
available s i e = n + round (eSurplus e Map.! (s,i))
 where
  (n, _) = randomR (0,3) (feedRGX (s,i) $ eProbabilityRGX e)
today :: Economy -> Day
today = eToday

{- Player actions.  -}
buy :: Settlement -> Item -> Int -> (Economy -> Economy)
buy s i n = sell s i (negate n)
sell :: Settlement -> Item -> Int -> (Economy -> Economy)
sell s i n = sellRational s i (fromIntegral n)

sellRational :: Settlement -> Item -> Rational -> (Economy -> Economy)
sellRational s i n e = nSurplus `seq`
  e
  { eSurplus = nSurplus
  }
 where
  nSurplus = Map.update (\n2 -> Just $ n2 + n)
                        (s,i)
                        (eSurplus e)

{------------------------------------------------------------------------------
Economy-manipulation Monad
------------------------------------------------------------------------------}

newtype EM a
  = EM
    { coreEM :: ReaderT (Ruleset,TMap,Difficulty,Economy) (Cont [EF]) a
    }
type EF = Economy -> Economy
instance Monad EM where
  return = EM . return
  fail = EM . fail
  EM ma >>= f = EM $ ma >>= \a -> coreEM (f a)
liftEF :: EF -> EM ()
liftEF f = EM $ lift $ withCont coreEF (return ())
 where
  coreEF k () = f:k()
runEM :: EM a -> (Ruleset,TMap,Difficulty) -> (Economy -> Economy)
runEM (EM ma) (rs, tm, di) e =	foldl' apply e fs
 where
  apply e f = f e
  fs = runCont (runReaderT ma (rs, tm, di, e)) (\_ -> [])
       `using` parBuffer 64 rseq

emGetSettlements :: EM [Settlement]
emGetSettlements = EM $ do
  (_,tm,_,_) <- ask
  return $ map (\ (s,_,_) -> s) $ settlementsTMap tm
emGetItems :: EM [Item]
emGetItems = EM $ do
  (rs,_,_,_) <- ask
  return $ RS.items rs
emGetDistance :: Settlement -> Settlement -> EM Distance
emGetDistance s1 s2 = EM $ do
  (_,tm,_,_) <- ask
  return $ distanceTMap tm s1 s2
emGetPrice :: Settlement -> Item -> EM Price
emGetPrice s i = EM $ do
  (_,_,_,e) <- ask
  return $ ePrice e Map.! (s,i)
emGetPorosity :: EM Rational
emGetPorosity = EM $ do
  (rs,_,di,_) <- ask
  return $ RS.permeability rs di
emSell :: Settlement -> Item -> Rational -> EM ()
emSell s i n = liftEF $ s `seq` i `seq` n `seq` sellRational s i n
emBuy :: Settlement -> Item -> Rational -> EM ()
emBuy s i n = emSell s i (negate n)
emGetCraftsmen :: Settlement -> EM [Craftsman]
emGetCraftsmen s = EM $ do
  (rs,tm,_,_) <- ask
  return $ RS.craftsmen rs (fst $ settlementLookupTMap tm s)
emGetSurplus :: Settlement -> Item -> EM Rational
emGetSurplus s i = EM $ do
  (_,_,_,e) <- ask
  return $ eSurplus e Map.! (s,i)
emGetCenterPrice :: EM Price
emGetCenterPrice = EM $ do
  (rs,_,di,_) <- ask
  return $ RS.centerPrice rs di
emGetKP :: EM Rational
emGetKP = EM $ do
  (rs,_,di,_) <- ask
  case RS.pidSettings rs di of
    (kp,ki,kd) -> return kp
emGetKI :: EM Rational
emGetKI = EM $ do
  (rs,_,di,_) <- ask
  case RS.pidSettings rs di of
    (kp,ki,kd) -> return ki
emGetKD :: EM Rational
emGetKD = EM $ do
  (rs,_,di,_) <- ask
  case RS.pidSettings rs di of
    (kp,ki,kd) -> return kd
emGetPreviousError :: Settlement -> Item -> EM Rational
emGetPreviousError s i = EM $ do
  (_,_,_,e) <- ask
  return $ ePreviousError e Map.! (s,i)
emGetIntegral :: Settlement -> Item -> EM Rational
emGetIntegral s i = EM $ do
  (_,_,_,e) <- ask
  return $ eIntegral e Map.! (s,i)
emSetPreviousError :: Settlement -> Item -> Rational -> EM ()
emSetPreviousError s i v = liftEF $ s `seq` i `seq` v `seq` \e ->
  e { ePreviousError = Map.insert (s,i) v $ ePreviousError e
    }
emSetIntegral :: Settlement -> Item -> Rational -> EM ()
emSetIntegral s i v = liftEF $ s `seq` i `seq` v `seq` \e ->
  e { eIntegral = Map.insert (s,i) v $ eIntegral e
    }
emSetPrice :: Settlement -> Item -> Price -> EM ()
emSetPrice s i v = liftEF $ s `seq` i `seq` v `seq` \e ->
  e { ePrice = Map.insert (s,i) v $ ePrice e
    }
emSetAvePrice :: Item -> Price -> EM ()
emSetAvePrice i v = liftEF $ i `seq` v `seq` \e ->
  e { eAvePrice = Map.insert i v $ eAvePrice e
    }
-- producers get positive indexes, consumers get negative
-- 0 is not a valid index.
emGetProducers :: Settlement -> EM [ProdConsId]
emGetProducers s = EM $ do
  (rs,tm,_,_) <- ask
  let ps = RS.producers rs (fst $ settlementLookupTMap tm s)
      len = fromIntegral $ length ps
  return $ map ProdConsId [1..len]
emGetConsumers :: Settlement -> EM [ProdConsId]
emGetConsumers s = EM $ do
  (rs,tm,_,_) <- ask
  let cs = RS.consumers rs (fst $ settlementLookupTMap tm s)
      len = fromIntegral $ length cs
  return $ map (ProdConsId . negate) [1..len]
emGetProdCons :: Settlement -> ProdConsId -> EM ProdCons
emGetProdCons s id = EM $ do
  (rs,tm,_,_) <- ask
  let ProdConsId numId = id
      lookup
        | numId < 0 = RS.consumers
        | otherwise = RS.producers
      numId' = abs numId - 1
      ps = lookup rs (fst $ settlementLookupTMap tm s)
  return $ ps !! fromIntegral numId'
emGetDay :: EM Day
emGetDay = EM $ do
  (_,_,_,e) <- ask
  return $ eToday e
emGetRGX :: Settlement -> ProdConsId -> EM RGX
emGetRGX s id = do
  prodcons <- emGetProdCons s id
  (_,_,_,e) <- EM $ ask
  return $ case prodcons of
             Scheduled _ _ _ -> feedRGX (s,id) $ eScheduledRGX e
             Probability _ _ -> feedRGX (s,id) $ eProbabilityRGX e
instance CMerchM EM where
  cmGetAllSettlements = emGetSettlements
  cmGetAllItems = emGetItems
  cmGetDistance = emGetDistance
  cmGetPrice = emGetPrice
  cmGetPorosity = emGetPorosity
  cmTransfer src dst num i = do
    emBuy src i num
    emSell dst i num
instance ItemSetPriceM EM where
  ispGetPrice = emGetPrice
instance CraftsmenM EM where
  crGetAllSettlements = emGetSettlements
  crGetAllCraftsmen = emGetCraftsmen
  crProduce s i n = emSell s i $ fromIntegral n
instance MarketM EM where
  mkGetAllSettlements = emGetSettlements
  mkGetAllItems = emGetItems
  mkGetSurplus = emGetSurplus
  mkGetCenterPrice = emGetCenterPrice
  mkGetKP = emGetKP
  mkGetKI = emGetKI
  mkGetKD = emGetKD
  mkGetPreviousError = emGetPreviousError
  mkSetPreviousError = emSetPreviousError
  mkGetIntegral = emGetIntegral
  mkSetIntegral = emSetIntegral
  mkSetTargetPrice = emSetPrice
instance ProdConsM EM where
  pcGetAllSettlements = emGetSettlements
  pcGetAllProducers = emGetProducers
  pcGetAllConsumers = emGetConsumers
  pcGetProdCons = emGetProdCons
  pcGetDay = emGetDay
  pcGetRGX = emGetRGX
  pcProduce s i n = emSell s i $ fromIntegral n

stepEM :: EM ()
stepEM = do
  cmerch
  craftsmen
  market
  prodcons
aveEM :: EM ()
aveEM = do
  items <- emGetItems
  settlements <- emGetSettlements
  forM_ items $ \i -> do
    prices <- forM settlements $ \s -> emGetPrice s i
    let ave = sum prices `div` (fromIntegral $ length prices)
    emSetAvePrice i ave
