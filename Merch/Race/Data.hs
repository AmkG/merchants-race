{- Merch.Race.Data - Common simple data types.

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
module Merch.Race.Data
  ( Settlement(..)
  , SettlementType(..)
  , Item(..)
  , Price(..)
  , Distance(..)
  , Day
  , Craftsman(..)
  , ItemSet(..)
  , ProdCons(..)
  , ProdConsId(..)
  , Terrain(..)
  , NameGenerator(..)
  , Difficulty(..)
  ) where

import Merch.Race.Data.Serialize

import Data.Ratio

newtype Settlement
  = Settlement String
  deriving (Eq, Ord, Show, Read)
newtype SettlementType
  = SettlementType String
  deriving (Eq, Ord, Show, Read)
newtype Item
  = Item String
  deriving (Eq, Ord, Show, Read)
newtype Price
  = Price Integer
  deriving (Eq, Ord, Show, Read)
newtype Distance
  = Distance Integer
  deriving (Eq, Ord, Show, Read)
newtype Day
  = Day Integer
  deriving (Eq, Ord, Show, Read)
data Craftsman
  = Craftsman
    { craftsmanInputs :: ItemSet
    , craftsmanOutputs :: ItemSet
    }
  deriving (Eq, Show, Read)
-- Item sets are either:
-- any {
--   # means, any of the following sets
--   { 1 "Item 1" 3 "Item 2" } # means, all these items
--   { 1 "Item 4" 3 "Item 5" }
--   { 2 "Item 3" }
-- }
-- or:
-- { 1 "Item 1" 2 "Item 4" 3 "Item 3" 12 "Item 2" }
-- the latter is simply a special case of the former
-- with a single set.
data ItemSet
  = ItemSet [(Integer, Item)] [[(Integer, Item)]]
  deriving (Eq, Ord, Show, Read)
data ProdCons
  = Scheduled Day Day ItemSet
  | Probability (Ratio Integer) ItemSet
  deriving (Show, Read)
newtype ProdConsId
  = ProdConsId Integer
  deriving (Eq, Ord, Show, Read)
newtype Difficulty
  = Difficulty String
  deriving (Eq, Ord, Show, Read)

{- Boilerplate.  Avoid using GeneralizedNewtypeDeriving to
   maintain H98 compatibility.  -}
instance Num Price where
  Price a + Price b = Price $ a + b
  Price a - Price b = Price $ a - b
  Price a * Price b = Price $ a * b
  negate (Price a) = Price $ negate a
  abs (Price a) = Price $ abs a
  signum (Price a) = Price $ signum a
  fromInteger i = Price $ fromInteger i
instance Real Price where
  toRational (Price a) = toRational a
instance Enum Price where
  succ (Price a) = Price $ succ a
  pred (Price a) = Price $ pred a
  toEnum i = Price $ toEnum i
  fromEnum (Price a) = fromEnum a
  enumFrom (Price a) = map Price $ enumFrom a
  enumFromThen (Price a) (Price a') = map Price $ enumFromThen a a'
  enumFromTo (Price a) (Price a') = map Price $ enumFromTo a a'
  enumFromThenTo (Price a) (Price a') (Price a'') = map Price $ enumFromThenTo a a' a''
instance Integral Price where
  Price a `quot` Price b = Price $ a `quot` b
  Price a `rem` Price b = Price $ a `rem` b
  Price a `div` Price b = Price $ a `div` b
  Price a `mod` Price b = Price $ a `mod` b
  Price a `quotRem` Price b = (Price q, Price r)
   where
     (q, r) = a `quotRem` b
  Price a `divMod` Price b = (Price q, Price r)
   where
     (q, r) = a `divMod` b
  toInteger (Price a) = toInteger a

instance Num Distance where
  Distance a + Distance b = Distance $ a + b
  Distance a - Distance b = Distance $ a - b
  Distance a * Distance b = Distance $ a * b
  negate (Distance a) = Distance $ negate a
  abs (Distance a) = Distance $ abs a
  signum (Distance a) = Distance $ signum a
  fromInteger i = Distance $ fromInteger i
instance Real Distance where
  toRational (Distance a) = toRational a
instance Enum Distance where
  succ (Distance a) = Distance $ succ a
  pred (Distance a) = Distance $ pred a
  toEnum i = Distance $ toEnum i
  fromEnum (Distance a) = fromEnum a
  enumFrom (Distance a) = map Distance $ enumFrom a
  enumFromThen (Distance a) (Distance a') = map Distance $ enumFromThen a a'
  enumFromTo (Distance a) (Distance a') = map Distance $ enumFromTo a a'
  enumFromThenTo (Distance a) (Distance a') (Distance a'') = map Distance $ enumFromThenTo a a' a''
instance Integral Distance where
  Distance a `quot` Distance b = Distance $ a `quot` b
  Distance a `rem` Distance b = Distance $ a `rem` b
  Distance a `div` Distance b = Distance $ a `div` b
  Distance a `mod` Distance b = Distance $ a `mod` b
  Distance a `quotRem` Distance b = (Distance q, Distance r)
   where
     (q, r) = a `quotRem` b
  Distance a `divMod` Distance b = (Distance q, Distance r)
   where
     (q, r) = a `divMod` b
  toInteger (Distance a) = toInteger a

instance Num Day where
  Day a + Day b = Day $ a + b
  Day a - Day b = Day $ a - b
  Day a * Day b = Day $ a * b
  negate (Day a) = Day $ negate a
  abs (Day a) = Day $ abs a
  signum (Day a) = Day $ signum a
  fromInteger i = Day $ fromInteger i
instance Real Day where
  toRational (Day a) = toRational a
instance Enum Day where
  succ (Day a) = Day $ succ a
  pred (Day a) = Day $ pred a
  toEnum i = Day $ toEnum i
  fromEnum (Day a) = fromEnum a
  enumFrom (Day a) = map Day $ enumFrom a
  enumFromThen (Day a) (Day a') = map Day $ enumFromThen a a'
  enumFromTo (Day a) (Day a') = map Day $ enumFromTo a a'
  enumFromThenTo (Day a) (Day a') (Day a'') = map Day $ enumFromThenTo a a' a''
instance Integral Day where
  Day a `quot` Day b = Day $ a `quot` b
  Day a `rem` Day b = Day $ a `rem` b
  Day a `div` Day b = Day $ a `div` b
  Day a `mod` Day b = Day $ a `mod` b
  Day a `quotRem` Day b = (Day q, Day r)
   where
     (q, r) = a `quotRem` b
  Day a `divMod` Day b = (Day q, Day r)
   where
     (q, r) = a `divMod` b
  toInteger (Day a) = toInteger a

data Terrain
 = Sea
 | Freshwater
 | Coast
 | Plains
 | Forest
 | Hill
 | Mountain
 deriving(Show, Read, Eq, Ord, Enum)

data NameGenerator
  = NGString String
  | NGConcat [NameGenerator]
  | NGDistribute [(NameGenerator, Int)]
  deriving(Show, Read)

{- Serialization.  -}
instance Serialize Settlement where
  hPut = hPutConvert $ \ (Settlement s) -> s
  hGet = hGetConvert $ \ s              -> (Settlement s)
instance Serialize SettlementType where
  hPut = hPutConvert $ \ (SettlementType s) -> s
  hGet = hGetConvert $ \ s                  -> (SettlementType s)
instance Serialize Item where
  hPut = hPutConvert $ \ (Item s) -> s
  hGet = hGetConvert $ \ s        -> (Item s)
instance Serialize Price where
  hPut = hPutConvert $ \ (Price i) -> i
  hGet = hGetConvert $ \ i         -> (Price i)
instance Serialize Distance where
  hPut = hPutConvert $ \ (Distance i) -> i
  hGet = hGetConvert $ \ i            -> (Distance i)
instance Serialize Day where
  hPut = hPutConvert $ \ (Day i) -> i
  hGet = hGetConvert $ \ i       -> (Day i)
instance Serialize ProdConsId where
  hPut = hPutConvert $ \ (ProdConsId i) -> i
  hGet = hGetConvert $ \ i              -> (ProdConsId i)
instance Serialize Difficulty where
  hPut = hPutConvert $ \ (Difficulty i) -> i
  hGet = hGetConvert $ \ i              -> (Difficulty i)
