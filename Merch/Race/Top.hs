
{- Top-level module for Merchant's Race.  -}

module Merch.Race.Top
  ( tryLoadResources
  , mainGame
  , GameResources
  ) where

import qualified Merch.Race.DrawingCombinators as Draw
import Merch.Race.Drawing
import Merch.Race.Ruleset(Ruleset)
import Merch.Race.Ruleset.Load

import Paths_merchrace

import Data.IORef
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=))

data GameResources
  = GR
    { grRuleset :: Ruleset
    , grFont :: Draw.Font
    }

tryLoadResources :: IO (Either String GameResources)
tryLoadResources = catch (loadResources >>= return . Right)
                         (return . Left . show)
loadResources :: IO GameResources
loadResources = do
  ruleset <- getDataFileName "ruleset" >>= loadRuleset
  font <- getDataFileName "FreeSans.ttf" >>= Draw.openFont
  return $ GR ruleset font

mainGame :: GameResources -> Screen
mainGame gr aspect = core
 where
  core (KeyDown _ '\ESC')  = GLUT.leaveMainLoop >> return NoTopReaction
  core _                   = return NoTopReaction
