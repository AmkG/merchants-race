
module Merch.Race.Data
  ( Settlement
  , Item
  , Price
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
