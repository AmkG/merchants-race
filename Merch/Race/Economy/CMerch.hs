{- Merch.Race.Economy.CMerch - Computer merchants model.

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

{- Non-player (computer) merchants model.  -}

{- Merchants move items from settlements with low price
   to settlements with high price.  If the base price of
   the item is cheapr, the merchant can move more items.  -}
module Merch.Race.Economy.CMerch
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
  cmGetPrice :: Settlement -> Item -> m Price
  -- A difficulty setting for the game.  The higher,
  -- the more efficient other merchants are and the
  -- smaller price differences are between setlements.
  cmGetPorosity :: m (Ratio Integer)
  -- Transfer items from the first settlement
  -- to the second settlement.
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

  forM_ pdis $ \ (s1, s2, dist, item) -> do
    -- items flow from settlement with lower price
    -- to settlement with higher price.
    price1 <- cmGetPrice s1 item
    price2 <- cmGetPrice s2 item
    let s1Cheaper = price1 < price2
        source
          | s1Cheaper = s1
          | otherwise = s2
        target
          | s1Cheaper = s2
          | otherwise = s1
        sourcePrice
          | s1Cheaper = price1
          | otherwise = price2
        targetPrice
          | s1Cheaper = price2
          | otherwise = price1
    let iSourcePrice = fromIntegral sourcePrice
        roi = fromIntegral targetPrice % iSourcePrice
        voltage = roi / (iSourcePrice % 1)
        resistance = fromIntegral dist % 1
        current = voltage / resistance

        num = porosity * current
        finalNum
          | num > 0   = num
          | otherwise = 0
    cmTransfer source target finalNum item

pairsOf :: [a] -> [(a, a)]
pairsOf []     = []
pairsOf (a:as) = pairWith a as ++ pairsOf as
 where
  pairWith a []     = []
  pairWith a (b:as) = (a,b):pairWith a as
