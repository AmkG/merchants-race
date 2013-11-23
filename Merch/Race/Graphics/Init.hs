
module Merch.Race.Graphics.Init
  ( initializeGraphics
  ) where

import Control.Exception

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=))

type GraphicsState = ()

initializeGraphics :: IO () -> IO ()
initializeGraphics act = bracket graphInit graphDeinit (\_ -> act)

-- default handlers
displayer :: GLUT.DisplayCallback
displayer = do
  GLUT.clear [GLUT.ColorBuffer]
  GLUT.flush
keyboarder :: GLUT.KeyboardMouseCallback
keyboarder _ GLUT.Down _ _ = do
  GLUT.leaveMainLoop
closer :: GLUT.CloseCallback
closer = do
  GLUT.leaveMainLoop
reshaper :: GLUT.ReshapeCallback
reshaper size = do
  GL.viewport $= (GLUT.Position 0 0, size)
  GLUT.postRedisplay Nothing

graphInit :: IO GraphicsState
graphInit = do
  GLUT.initialize "Merchant's Race" []
  GLUT.createWindow "Merchant's Race"
  GLUT.displayCallback $= displayer
  GLUT.keyboardMouseCallback $= Just keyboarder
  GLUT.closeCallback $= Just closer
  GLUT.reshapeCallback $= Just reshaper
  GLUT.actionOnWindowClose $= GLUT.ContinueExectuion
  GLUT.fullScreen
  return ()

graphDeinit :: GraphicsState -> IO ()
graphDeinit st = do
  return ()
