{- Non-player (computer) merchants model.  -}

{- We model the other merchants as a voltage network.
   We consider the difference in surpluses between
   each pair of settlements as the "raw" voltage; the
   settlement with higher surplus has a higher voltage
   and items flow from the higher surplus to the lower
   surplus.  Since merchants are money-grubbers, the
   raw voltage is multiplied by the return on
   investment (the price at the target divided by the
   price at the source).  The resistance is then the
   distance between the settlements; dividing the
   voltage by the resistance yields a current flow of
   items.  -}
module Merch.Race.CMerch
  ( CMerchM(..)
  , cmerch
  ) where

import Merch.Race.Data

import Control.Monad
import Data.Ratio

class Monad m => CMerchM m where
  cmGetAllSettlements :: m [Settlement]
  cmGetAllItems :: m [Item]
  cmGetDistance :: Settlement -> Settlement -> m Distance
  cmGetSurplus :: Settlement -> Item -> m (Ratio Integer)
  cmGetPrice :: Settlement -> Item -> m Price
  -- A difficulty setting for the game.  The higher,
  -- the more efficient other merchants are and the
  -- smaller price differences are between setlements.
  cmGetPorosity :: m (Ratio Integer)
  cmTransfer :: Settlement -> Settlement -> Ratio Integer -> Item -> m ()

cmerch :: CMerchM m => m ()
cmerch = do
  settlements <- cmGetAllSettlements
  items <- cmGetAllItems
  porosity <- cmGetPorosity

  let pairs = pairsOf settlements
  distances <- mapM (uncurry cmGetDistance) pairs
  let pds = zip pairs distances
      pdis = [(s1, s2, d, i) | ((s1, s2), d) <- pds, i <- items]

  actions <- forM pdis $ \ (s1, s2, dist, item) -> do
    surplus1 <- cmGetSurplus s1 item
    surplus2 <- cmGetSurplus s2 item
    let s1Higher = surplus1 > surplus2
        source
          | s1Higher  = s1
          | otherwise = s2
        target
          | s1Higher  = s2
          | otherwise = s1
        sourceSurplus
          | s1Higher  = surplus1
          | otherwise = surplus2
        targetSurplus
          | s1Higher  = surplus2
          | otherwise = surplus1
    sourcePrice <- cmGetPrice source item
    targetPrice <- cmGetPrice target item
    let roi = fromIntegral targetPrice % fromIntegral sourcePrice
        voltage = roi * (sourceSurplus - targetSurplus)
        resistance = fromIntegral dist % 1
        current = voltage / resistance

        num = porosity * current
    if (num > 0)
      then return (cmTransfer source target num item)
      else return (return ())

  sequence_ actions

pairsOf :: [a] -> [(a, a)]
pairsOf []     = []
pairsOf (a:as) = pairWith a as ++ pairsOf as
 where
  pairWith a []     = []
  pairWith a (b:as) = (a,b):pairWith a as
