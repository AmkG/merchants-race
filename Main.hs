
module Main(main) where

import qualified Merch.Race.UI.DrawingCombinators as Draw
import Merch.Race.UI.Drawing
import Merch.Race.Top

import Paths_merchrace

import Data.List
import Data.Monoid
import Graphics.DrawingCombinators(Image, (%%))
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=))
import System.Exit

core :: () -> IO ()
core () = do
  ers <- tryLoadResources
  case ers of
    Left e  -> do
      catch (do font <- getDataFileName "FreeSans.ttf" >>= Draw.openFont
                reportError font e)
            (\_ -> do putStrLn e)
    Right r -> initialScreen $ mainGame r

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
  -- Create the drawing.
      topdrawing = drawingStatic top
      screenfunc aspect ReDo                = return $ SetDrawing topdrawing
      screenfunc aspect (KeyDown _ '\ESC')  = GLUT.leaveMainLoop >> return NoTopReaction
      screenfunc aspect _                   = return NoTopReaction
  -- Display the error message.
  initialScreen screenfunc

main = core ()
