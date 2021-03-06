{- Merch.Race.Ruleset.Data - Representation of Rulesets.

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
{- Data declaration of ruleset.  -}

module Merch.Race.Ruleset.Data
  ( Ruleset(..)
  , emptyRuleset
  , SettlementTypeDesc(..)
  , emptySettlementTypeDesc
  , DifficultyDesc(..)
  , emptyDifficultyDesc
  ) where

import Merch.Race.Data

import qualified Data.Map as Map
import Data.Map(Map)

data Ruleset
  = Ruleset
    { itemmap :: Map Item String
    , settlementtypemap :: Map SettlementType SettlementTypeDesc
    , namegeneratormaybe :: Maybe NameGenerator
    , settlementgeneratorlist :: [(Int, SettlementType)]
    , difficultymap :: Map Difficulty DifficultyDesc
    }
  deriving (Show, Read)
emptyRuleset = Ruleset
               { itemmap = Map.empty
               , settlementtypemap = Map.empty
               , namegeneratormaybe = Nothing
               , settlementgeneratorlist = []
               , difficultymap = Map.empty
               }

data SettlementTypeDesc
  = SettlementTypeDesc
    { stname :: String
    , stdesc :: String
    , stproducers :: [ProdCons]
    , stconsumers :: [ProdCons]
    , stcraftsmen :: [Craftsman]
    , stterrain :: [Terrain]
    }
  deriving (Show, Read)
emptySettlementTypeDesc :: SettlementTypeDesc
emptySettlementTypeDesc =
  SettlementTypeDesc [] [] [] [] [] []

data DifficultyDesc
  = DifficultyDesc
    { difficultyname :: String
    , difficultydesc :: String
    , cartupgradecost :: (Rational, ItemSet)
    , supplycost :: (Rational, ItemSet)
    , inncost :: (Rational, ItemSet)
    , marketlunchcost :: (Rational, ItemSet)
    , publunchcost :: (Rational, ItemSet)
    , banditprobability :: Map Terrain Rational
    , permeability :: Rational
    , interestrate :: Rational
    , centerprice :: Price
    , pidsettings :: (Rational, Rational, Rational) -- kp ki kd
    , startingloan :: Price
    }
  deriving (Show, Read)
emptyDifficultyDesc = DifficultyDesc
                      { difficultyname = ""
                      , difficultydesc = ""
                      , cartupgradecost = (0, ItemSet [] [])
                      , supplycost = (0, ItemSet [] [])
                      , inncost = (0, ItemSet [] [])
                      , marketlunchcost = (0, ItemSet [] [])
                      , publunchcost = (0, ItemSet [] [])
                      , banditprobability = Map.empty
                      , permeability = 0.1
                      , interestrate = 0.1
                      , centerprice = 500
                      , pidsettings = (0.1,0.5,0.001)
                      , startingloan = 1000
                      }
