
{- Top-level module for Merchant's Race.  -}

module Merch.Race.Top
  ( tryLoadResources
  , mainGameLoop
  , GameResources
  ) where

import qualified Merch.Race.DrawingCombinators as Draw
import Merch.Race.Graphics
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

mainGameLoop :: GameResources -> IO ()
mainGameLoop gr = do
  -- TODO: make a useable graphics system.
  ptvar <- newIORef (0,0)
  let mouser pt = do
        writeIORef ptvar pt
        GLUT.postRedisplay Nothing
      displayer _ = do
        pt <- readIORef ptvar
        let im = Draw.line (0,0) pt
        Draw.render im
  displayFunc $= displayer
  mouseMoveFunc $= mouser

  GLUT.mainLoop
  return ()
