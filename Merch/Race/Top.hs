
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
  mkButton = button bc
  exitButton = mkButton (0, -0.65) "Exit" exitScreen
  newButton  = mkButton (0, 0.6) "New Game"   (notImplemented gr coreScreen)
  loadButton = mkButton (0, 0.3) "Load Game"  (notImplemented gr coreScreen)
  hiButton   = mkButton (0, 0.0) "High Score" (notImplemented gr coreScreen)
  buttons = mconcat [exitButton, newButton, loadButton, hiButton]

  top = buttons

  core (KeyDown _ '\ESC')  = GLUT.leaveMainLoop >> return NoTopReaction
  core ReDo                = do
    return $ SetDrawing top
  core _                   = return NoTopReaction

  coreScreen aspect'
    | aspect == aspect' = core
    | otherwise         = \_ -> return $ SetScreen $ mainGame gr

notImplemented :: GameResources -> Screen -> Screen
notImplemented gr src aspect = core
 where
  font = grFont gr
  bc = mkButtonConfig [ButtonFont font]
  exitButton = button bc (0, -0.75) "Exit" src
  msgText = "This part is not yet implemented!"
  message = Draw.text font msgText
  width = Draw.textWidth font msgText
  messageCentered = Draw.translate (-1, 0)
                 %% Draw.scale (2/width) (2/width)
                 %% message

  top = mconcat
        [ exitButton
        , drawingStatic messageCentered
        ]

  core (KeyDown _ '\ESC')  = return $ SetScreen src
  core ReDo                = return $ SetDrawing top
  core _                   = return NoTopReaction

exitScreen :: Screen
exitScreen aspect _  = GLUT.leaveMainLoop >> return NoTopReaction
