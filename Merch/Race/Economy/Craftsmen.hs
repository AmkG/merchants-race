{- Merch.Race.Economy.Craftsmen - Model for craftsmen.

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
{- Craftsmen model.  -}

{- Craftsmen have a simple model: if the total price
   of their output items is greater than the total
   price of their input items, they consume their
   input and produce their output.  Otherwise, they
   twiddle their thumbs and do nothing.

   Given the option, they will consume the cheapest
   ingredients to produce the most expensive results.  -}
module Merch.Race.Economy.Craftsmen
  ( CraftsmenM(..)
  , craftsmen
  ) where

import Merch.Race.Data
import Merch.Race.Economy.ItemSetPrice

import Control.Monad
import Data.List

class ItemSetPriceM m => CraftsmenM m where
  crGetAllSettlements :: m [Settlement]
  crGetAllCraftsmen :: Settlement -> m [Craftsman]
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
      inputsPrice <- lowestItemSetPrice settlement inputs
      outputsPrice <- highestItemSetPrice settlement outputs
      let (inputs, inputsBestPrice) = inputsPrice
          (outputs, outputsBestPrice) = outputsPrice
      when (inputsBestPrice < outputsBestPrice) $ do
        forM_ inputs $ \ (num, item) -> crProduce settlement item (negate num)
        forM_ outputs $ \ (num, item) -> crProduce settlement item num

