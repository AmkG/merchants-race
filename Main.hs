
module Main(main) where

import Merch.Race.Graphics.Init
import qualified Merch.Race.Ruleset as Ruleset
import Merch.Race.Ruleset(Ruleset)
import Merch.Race.Ruleset.Load

import Paths_merchrace

import Data.List
import Data.Monoid
import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators(Image, (%%))
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=))
import System.Exit
import System.IO.Error

-- attempt to load the ruleset.
tryLoadRuleset :: IO (Either String Ruleset)
tryLoadRuleset = do
  dir <- getDataFileName "ruleset"
  catch (loadRuleset dir >>= return . Right)
        (return . Left . show)

core :: IO ()
core = do
  ers <- tryLoadRuleset
  fontfile <- getDataFileName "FreeSans.ttf"
  font <- Draw.openFont fontfile
  case ers of
    Left e  -> reportError font e
    Right r -> gameLoop font r

-- Report an error loading ruleset
reportError :: Draw.Font -> String -> IO ()
reportError font e = do
  -- TODO: Display the error in the GUI
  let elines = lines e
      rawtexts = map (Draw.text font) elines
      texts = map translate $ zip rawtexts [0..]
       where
         translate (text, n) = Draw.translate (0, -2 * fromIntegral n) %% text
      textwidths = map (Draw.textWidth font) elines
      textwidth = foldl' max 0.1 textwidths
      text = foldl' mappend mempty texts
      scaledtext = Draw.scale (1/textwidth) (1/textwidth) %% text
      top = Draw.translate (-0.5, 0) %% scaledtext
      display = do
        GLUT.clear [GLUT.ColorBuffer]
        Draw.render top
        GLUT.flush
  GLUT.displayCallback $= display
  GLUT.mainLoop
  exitWith $ ExitFailure 1

-- Main game loop
gameLoop :: Draw.Font -> Ruleset -> IO ()
gameLoop font r = do
  -- TODO: Setup callbacks
  let top = Draw.text font "Merchant's Race"
      display = do
        GLUT.clear [GLUT.ColorBuffer]
        Draw.render top
        GLUT.flush
  GLUT.displayCallback $= display
  GLUT.mainLoop
  return ()

main = initializeGraphics core
