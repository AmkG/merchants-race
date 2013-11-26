
{- Top-level module for Merchant's Race.  -}

module Merch.Race.Top
  ( tryLoadResources
  , mainGame
  , GameResources
  ) where

import Merch.Race.Ruleset(Ruleset)
import Merch.Race.Ruleset.Load
import Merch.Race.UI.Button
import qualified Merch.Race.UI.DrawingCombinators as Draw
import Merch.Race.UI.DrawingCombinators((%%))
import Merch.Race.UI.Drawing

import Paths_merchrace

import Data.IORef
import Data.Monoid
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
  bc = mkButtonConfig [ButtonFont $ grFont gr]
  core (KeyDown _ '\ESC')  = GLUT.leaveMainLoop >> return NoTopReaction
  core ReDo                = do
    let disp = button bc (0, -0.75) "Exit" exitScreen
    return $ SetDrawing disp
  core _                   = return NoTopReaction

exitScreen :: Screen
exitScreen aspect _  = GLUT.leaveMainLoop >> return NoTopReaction
