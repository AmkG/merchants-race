
module Main(main) where

import Merch.Race.Graphics.Init
import qualified Merch.Race.Ruleset as Ruleset
import Merch.Race.Ruleset(Ruleset)
import Merch.Race.Ruleset.Load

import Paths_merchrace

import qualified Graphics.UI.GLUT as GLUT
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
  case ers of
    Left e  -> reportError e
    Right r -> gameLoop r

-- Report an error loading ruleset
reportError :: String -> IO ()
reportError e = do
  -- TODO: Display the error in the GUI
  putStrLn "Error loading ruleset"
  putStrLn e

  exitWith $ ExitFailure 1

-- Main game loop
gameLoop :: Ruleset -> IO ()
gameLoop r = do
  -- TODO: Actually operate the game
  putStrLn "Merchant's Race!"
  GLUT.mainLoop
  return ()

main = initializeGraphics core
