
module Merch.Race.ItemSetPrice
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

