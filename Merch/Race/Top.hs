{- Merch.Race.Top - Main menu and main game code.

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
{- Top-level module for Merchant's Race.  -}

module Merch.Race.Top
  ( tryLoadResources
  , mainGame
  , GameResources
  ) where

import Prelude hiding(catch)

import Merch.Race.GameResources
import Merch.Race.Ruleset(Ruleset)
import Merch.Race.Ruleset.Load
import Merch.Race.Top.MapGen
import Merch.Race.UI.Button
import qualified Merch.Race.UI.DrawingCombinators as Draw
import Merch.Race.UI.DrawingCombinators((%%))
import Merch.Race.UI.Drawing
import Merch.Race.UI.Minimap

import Paths_merchrace

import Control.Exception
import Data.IORef
import Data.Monoid
import qualified Data.Set as Set
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=))

tryLoadResources :: IO (Either String GameResources)
tryLoadResources = catch (loadResources >>= return . Right)
                         (return . Left . (show :: SomeException -> String))
loadResources :: IO GameResources
loadResources = do
  ruleset <- getDataFileName "ruleset" >>= loadRuleset
  font <- getDataFileName "FreeSans.ttf" >>= Draw.openFont
  return $ GameResources ruleset font

mainGame :: GameResources -> Screen
mainGame gr aspect = core
 where
  bc = mkButtonConfig [ButtonFont $ grFont gr]
  mkButton = button bc
  exitButton = mkButton (0, -0.65) "Exit" exitScreen
  newButton  = mkButton (0, 0.6) "New Game"   $ newGameScreen gr
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

newGameScreen :: GameResources -> Screen
newGameScreen gr _ _ = return $ SetScreen screen1
 where
  screen1 = mapgenScreen gr screen2 (mainGame gr)
  screen2 tmap = displayTMap tmap

  -- Temporary screen to just display the resulting map.
  displayTMap tmap aspect = core
   where
    bc = mkButtonConfig [ButtonFont $ grFont gr]
    drawing = drawingStatic $ minimap tmap Set.empty
    core ReDo               = return $ SetDrawing drawing
    core (KeyDown _ '\ESC') = return $ SetScreen $ mainGame gr
    core _                  = return NoTopReaction
