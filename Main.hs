
module Main(main) where

import Merch.Race.Graphics
import Merch.Race.Top

import Paths_merchrace

import Data.List
import Data.Monoid
import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators(Image, (%%))
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=))
import System.Exit

core :: IO ()
core = do
  ers <- tryLoadResources
  case ers of
    Left e  -> do
      catch (do font <- getDataFileName "FreeSans.ttf" >>= Draw.openFont
                reportError font e)
            (\_ -> do putStrLn e)
    Right r -> mainGameLoop r

-- Report an error loading ruleset
reportError :: Draw.Font -> String -> IO ()
reportError font e = do
  -- Create the error message image.
  let elines = lines (e ++ "\n(Press ESCAPE to exit)")
      rawtexts = map (Draw.text font) elines
      texts = map translate $ zip rawtexts [0..]
       where
         translate (text, n) = Draw.translate (0, -2 * fromIntegral n) %% text
      textwidths = map (Draw.textWidth font) elines
      textwidth = foldl' max 0.1 textwidths
      text = foldl' mappend mempty texts
      scaledtext = Draw.scale (2/textwidth) (2/textwidth) %% text
      top = Draw.translate (-1, 0) %% scaledtext
  -- Display the error message.
  let display aspect = do
        Draw.render top
  displayFunc $= display
  GLUT.mainLoop
  exitWith $ ExitFailure 1

main = initializeGraphics core
