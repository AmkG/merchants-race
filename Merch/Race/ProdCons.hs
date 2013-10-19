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

module Merch.Race.ProdCons
  ( ProdConsM(..)
  , prodcons
  ) where

import Merch.Race.Data
import Merch.Race.ItemSetPrice

import Control.Monad
import Data.Ratio

class ItemSetPriceM m => ProdConsM m where
  pcGetAllSettlements :: m [Settlement]
  pcGetAllProducers :: Settlement -> m [ProdConsId]
  pcGetAllConsumers :: Settlement -> m [ProdConsId]
  pcGetProdCons :: Settlement -> ProdConsId -> m ProdCons
  -- Get the current day of the year.
  pcGetDay :: m Day
  -- Get a random Integer from 0 to n - 1.
  pcGetRandom :: Integer -> m Integer
  -- Scheduled days
  -- The prodcons function promises to only call
  -- pcGetScheduledDay on ProdConsId's it has called
  -- pcSetScheduledDay previously.
  pcSetScheduledDay :: Settlement -> ProdConsId -> Day -> m ()
  pcGetScheduledDay :: Settlement -> ProdConsId -> m Day
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

  -- On day 1, schedule everything for this year.
  when (today == 1) $ do
    let settlementProdCons = settlementProducers ++ settlementConsumers
    forM_ settlementProdCons $ \ (settlement, prodcons) -> do
      forM_ prodcons $ \ prodcon -> do
        dat <- pcGetProdCons settlement prodcon
        case dat of
          Scheduled day variance _ -> do
            rand <- pcGetRandom (fromIntegral variance * 2 + 1)
            let targetDay = day + fromIntegral rand - variance
                selectDay
                  | targetDay < 1 = 1
                  | otherwise     = targetDay
            pcSetScheduledDay settlement prodcon selectDay
          _                        -> return ()

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
      (sel, itemset) <- case dat of
        Scheduled _ _ itemset -> do
          day <- pcGetScheduledDay settlement pcid
          return ((day == today), itemset)
        Probability prob itemset -> do
          rand <- pcGetRandom (denominator prob)
          return (rand < numerator prob, itemset)
      when sel $ do
        (items, _) <- selector settlement itemset
        forM_ items $ \ (num, item) -> do
          pcProduce settlement item (transform num)

