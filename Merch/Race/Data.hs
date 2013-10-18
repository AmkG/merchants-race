
module Merch.Race.Data
  ( Settlement(..)
  , Item(..)
  , Price(..)
  , Distance(..)
  , Craftsman(..)
  , ItemSet(..)
  ) where

newtype Settlement
  = Settlement String
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
data Craftsman
  = Craftsman
    { craftsmanInputs :: [(Integer, Item)]
    , craftsmanOutputs :: [(Integer, Item)]
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

