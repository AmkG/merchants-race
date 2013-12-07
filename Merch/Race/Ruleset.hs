{- Merch.Race.Ruleset - Ruleset-handling code.

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

{- This module is intended to be imported qualified.  -}
module Merch.Race.Ruleset
  ( Ruleset

  , items
  , settlementTypes
  , difficulties

  , producers
  , consumers
  , craftsmen
  , terrain

  , nameGenerator
  , settlementGenerator

  , cartUpgradeCost
  , supplyCost
  , innCost
  , marketLunchCost
  , pubLunchCost
  , bandits
  , permeability
  , interestRate
  , centerPrice
  , pidSettings
  , startingLoan

  , describe

  ) where

import Merch.Race.Data
import qualified Merch.Race.Ruleset.Data as D
import Merch.Race.Ruleset.Data(Ruleset)

import qualified Data.Map as Map
import Data.Map(Map)

items :: Ruleset -> [Item]
items r = Map.keys $ D.itemmap r
settlementTypes :: Ruleset -> [SettlementType]
settlementTypes r = Map.keys $ D.settlementtypemap r
difficulties :: Ruleset -> [Difficulty]
difficulties r = Map.keys $ D.difficultymap r

lookupSettlementType func r k =
  case Map.lookup k (D.settlementtypemap r) of
    Just d -> func d
producers :: Ruleset -> SettlementType -> [ProdCons]
producers = lookupSettlementType D.stproducers
consumers :: Ruleset -> SettlementType -> [ProdCons]
consumers = lookupSettlementType D.stconsumers
craftsmen :: Ruleset -> SettlementType -> [Craftsman]
craftsmen = lookupSettlementType D.stcraftsmen
terrain :: Ruleset -> SettlementType -> [Terrain]
terrain = lookupSettlementType D.stterrain

nameGenerator :: Ruleset -> NameGenerator
nameGenerator r =
  case D.namegeneratormaybe r of
    Just n  -> n
    Nothing -> NGString ""
settlementGenerator :: Ruleset -> [(Int, SettlementType)]
settlementGenerator = D.settlementgeneratorlist

lookupDifficulty func r k =
  case Map.lookup k (D.difficultymap r) of
    Just d -> func d
cartUpgradeCost :: Ruleset -> Difficulty -> (Rational, ItemSet)
cartUpgradeCost = lookupDifficulty D.cartupgradecost
supplyCost :: Ruleset -> Difficulty -> (Rational, ItemSet)
supplyCost = lookupDifficulty D.supplycost
innCost :: Ruleset -> Difficulty -> (Rational, ItemSet)
innCost = lookupDifficulty D.inncost
marketLunchCost :: Ruleset -> Difficulty -> (Rational, ItemSet)
marketLunchCost = lookupDifficulty D.marketlunchcost
pubLunchCost :: Ruleset -> Difficulty -> (Rational, ItemSet)
pubLunchCost = lookupDifficulty D.publunchcost
bandits :: Ruleset -> Difficulty -> Map Terrain Rational
bandits = lookupDifficulty D.banditprobability
permeability :: Ruleset -> Difficulty -> Rational
permeability = lookupDifficulty D.permeability
interestRate :: Ruleset -> Difficulty -> Rational
interestRate = lookupDifficulty D.interestrate
centerPrice :: Ruleset -> Difficulty -> Price
centerPrice = lookupDifficulty D.centerprice
pidSettings :: Ruleset -> Difficulty -> (Rational, Rational, Rational)
pidSettings = lookupDifficulty D.pidsettings
startingLoan :: Ruleset -> Difficulty -> Price
startingLoan = lookupDifficulty D.startingloan

-- Types that can be described in a ruleset.
class Describable i where
  describe :: Ruleset -> i -> String
instance Describable Item where
  describe r i =
    case Map.lookup i $ D.itemmap r of
      Just s -> s
instance Describable SettlementType where
  describe r i =
    case Map.lookup i $ D.settlementtypemap r of
      Just d -> D.stdesc d
instance Describable Difficulty where
  describe r i =
    case Map.lookup i $ D.difficultymap r of
      Just d -> D.difficultydesc d

