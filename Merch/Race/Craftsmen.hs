{- Craftsmen model.  -}

{- Craftsmen have a simple model: if the total price
   of their output items is greater than the total
   price of their input items, they consume their
   input and produce their output.  Otherwise, they
   twiddle their thumbs and do nothing.  -}
module Merch.Race.Craftsmen
  ( CraftsmenM(..)
  , craftsmen
  ) where

import Merch.Race.Data

import Control.Monad
import Data.List

class Monad m => CraftsmenM m where
  crGetAllSettlements :: m [Settlement]
  crGetAllCraftsmen :: Settlement -> m [Craftsman]
  crGetPrice :: Settlement -> Item -> m Price
  -- The function below is given negative values to
  -- consume the relevant item.
  crProduce :: Settlement -> Item -> Integer -> m ()

craftsmen :: CraftsmenM m => m ()
craftsmen = do
  settlements <- crGetAllSettlements
  forM_ settlements $ \settlement -> do
    craftsmen <- crGetAllCraftsmen settlement
    forM_ craftsmen $ \craftsman -> do
      let inputs = craftsmanInputs craftsman
          outputs = craftsmanOutputs craftsman
      inputsPrice <- getTotalPrice settlement inputs
      outputsPrice <- getTotalPrice settlement outputs
      when (inputsPrice < outputsPrice) $ do
        forM_ inputs $ \ (num, item) -> crProduce settlement item (negate num)
        forM_ outputs $ \ (num, item) -> crProduce settlement item num

getTotalPrice :: CraftsmenM m => Settlement -> [(Integer, Item)] -> m Price
getTotalPrice settlement items = do
  totalPrices <- forM items $ \ (num, item) -> do
    price <- crGetPrice settlement item
    return (fromIntegral num * price)
  return (foldl' (+) 0 totalPrices)
