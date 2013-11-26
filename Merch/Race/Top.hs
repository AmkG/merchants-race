
{- Top-level module for Merchant's Race.  -}

module Merch.Race.Top
  ( tryLoadResources
  , mainGame
  , GameResources
  ) where

import qualified Merch.Race.UI.DrawingCombinators as Draw
import Merch.Race.UI.DrawingCombinators((%%))
import Merch.Race.UI.Drawing
import Merch.Race.Ruleset(Ruleset)
import Merch.Race.Ruleset.Load

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
  core (KeyDown _ '\ESC')  = GLUT.leaveMainLoop >> return NoTopReaction
  core ReDo                = do
    let disp = mconcat [reacting (-0.5), reacting 0.5]
    return $ SetDrawing disp
  core _                   = return NoTopReaction

reacting :: Draw.R -> Drawing
reacting y = idleState
 where
  boxRaw = Draw.scale 0.25 0.25 %% Draw.regularPoly 4
  boxMoved = Draw.translate (0.0, y) %% boxRaw
  gray = Draw.Color 0.5 0.5 0.5 1
  white = Draw.Color 1 1 1 1
  yellow = Draw.Color 1 1 0.5 1

  idleBox = Draw.tint gray boxMoved
  activeBox = Draw.tint white boxMoved
  pressBox = Draw.tint yellow boxMoved

  idleStateF MouseMove    = [Modify activeState]
  idleStateF _            = []
  idleState = drawing idleStateF idleBox

  activeStateF MouseMoveOut = [Modify idleState]
  activeStateF MouseDown    = [Grab, Modify pressState]
  activeStateF _            = []
  activeState = drawing activeStateF activeBox

  pressStateF MouseMoveOut = [Modify pressOffState]
  pressStateF MouseUp      = [Ungrab, Replace exitScreen]
  pressStateF _            = []
  pressState = drawing pressStateF pressBox

  pressOffStateF MouseMove = [Modify pressState]
  pressOffStateF MouseUp   = [Ungrab, Modify idleState]
  pressOffStateF _         = []
  pressOffState = drawing pressOffStateF idleBox

  exitScreen :: Screen
  exitScreen _ _ = GLUT.leaveMainLoop >> return NoTopReaction
