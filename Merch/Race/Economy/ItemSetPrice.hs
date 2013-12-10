{- Merch.Race.Economy.ItemSetPrice - Compute prices of sets of items.

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
module Merch.Race.Economy.ItemSetPrice
  ( ItemSetPriceM(..)
  , highestItemSetPrice
  , lowestItemSetPrice
  ) where

import Merch.Race.Data

import Control.Monad
import Data.List

class Monad m => ItemSetPriceM m where
  ispGetPrice :: Settlement -> Item -> m Price

highestItemSetPrice :: ItemSetPriceM m => Settlement -> ItemSet -> m ([(Integer, Item)], Price)
highestItemSetPrice s (ItemSet set1 [])   = itemsPrice s set1
highestItemSetPrice s (ItemSet set1 sets) = do
  setPrices <- mapM (itemsPrice s) $ set1:sets
  let sortedSetPrices = sortBy (\ (_, a) (_, b) -> compare b a) setPrices
      best = head sortedSetPrices
  return best
lowestItemSetPrice :: ItemSetPriceM m => Settlement -> ItemSet -> m ([(Integer, Item)], Price)
lowestItemSetPrice s (ItemSet set1 [])   = itemsPrice s set1
lowestItemSetPrice s (ItemSet set1 sets) = do
  setPrices <- mapM (itemsPrice s) $ set1:sets
  let sortedSetPrices = sortBy (\ (_, a) (_, b) -> compare a b) setPrices
      best = head sortedSetPrices
  return best

itemsPrice :: ItemSetPriceM m => Settlement -> [(Integer, Item)] -> m ([(Integer, Item)], Price)
itemsPrice s items = do
  prices <- forM items $ \ (num, item) -> do
    price <- ispGetPrice s item
    let totalPrice = price * fromIntegral num
    return totalPrice
  let totalPrice = foldl' (+) 0 prices
  return (items, totalPrice)

