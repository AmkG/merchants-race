
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
