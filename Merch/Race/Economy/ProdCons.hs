{- Merch.Race.Economy.ProdCons - Model for item producers and consumers.

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

{- Model for producers and consumers.  -}

{- Item producers, given a choice, will try to produce
   the most expensive items, while consumers will try
   to consume the cheapest items.

   Item producers and consumers can be either
   probabilistic producers/consumers (having a
   probability of producing/consuming each day)
   or scheduled (will produce or consume on a
   specific day of the year, give or take some
   variance).  The latter is mostly to model
   farming village harvests.  -}

module Merch.Race.Economy.ProdCons
  ( ProdConsM(..)
  , prodcons
  ) where

import Merch.Race.Data
import Merch.Race.Economy.ItemSetPrice
import Merch.Race.RGX

import Control.Monad
import Data.Ratio
import System.Random

class ItemSetPriceM m => ProdConsM m where
  pcGetAllSettlements :: m [Settlement]
  pcGetAllProducers :: Settlement -> m [ProdConsId]
  pcGetAllConsumers :: Settlement -> m [ProdConsId]
  pcGetProdCons :: Settlement -> ProdConsId -> m ProdCons
  -- Get the current day of the year.
  pcGetDay :: m Day

  -- Get a random number generator for a specific ProdConsId.
  pcGetRGX :: Settlement -> ProdConsId -> m RGX

  -- Produce or consume.  Give negative values to
  -- consume instead.
  pcProduce :: Settlement -> Item -> Integer -> m ()

prodcons :: ProdConsM m => m ()
prodcons = do
  settlements <- pcGetAllSettlements
  today <- pcGetDay

  settlementProducers <- forM settlements $ \settlement -> do
    producers <- pcGetAllProducers settlement
    return (settlement, producers)
  settlementConsumers <- forM settlements $ \settlement -> do
    consumers <- pcGetAllConsumers settlement
    return (settlement, consumers)

  -- Producers and consumers are handled similarly,
  -- except for this bit about whether they go for
  -- the highest or the lowest price, and except
  -- for negating the numbers.
  let prodFuns = zip settlementProducers (repeat (highestItemSetPrice, id))
      consFuns = zip settlementConsumers (repeat (lowestItemSetPrice, negate))
      allFuns = prodFuns ++ consFuns
  forM_ allFuns $ \ ((settlement, pcids), (selector, transform)) -> do
    forM_ pcids $ \ pcid -> do
      dat <- pcGetProdCons settlement pcid
      g <- pcGetRGX settlement pcid
      (sel, itemset) <- case dat of
        Scheduled day off itemset -> do
          let day' = fromIntegral $ day :: Int
              off' = fromIntegral $ off
              (selOff', _) = randomR (negate off', off') g
              selDay = fromIntegral $ day' + selOff'
          return ((selDay == today), itemset)
        Probability prob itemset -> do
          let g2 = feedRGX (show today) g
              denom = fromIntegral $ denominator prob :: Int
              rand = fromIntegral $ fst $ randomR (0, denom - 1) g
          return (rand < numerator prob, itemset)
      when sel $ do
        (items, _) <- selector settlement itemset
        forM_ items $ \ (num, item) -> do
          pcProduce settlement item (transform num)

