{- Merch.Race.GameResources - Data definition for main game resources.

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
module Merch.Race.GameResources
  ( GameResources(..)
  ) where

import Merch.Race.Ruleset
import Merch.Race.UI.DrawingCombinators

data GameResources
  = GameResources
    { grRuleset :: Ruleset
    , grFont :: Font
    }
