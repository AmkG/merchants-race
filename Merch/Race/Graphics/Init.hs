
module Merch.Race.Graphics.Init
  ( initializeGraphics
  ) where

import Control.Exception

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

graphInit :: IO GraphicsState
graphInit = do
  GLUT.initialize "Merchant's Race" []
  GLUT.createWindow "Merchant's Race"
  GLUT.displayCallback $= displayer
  GLUT.keyboardMouseCallback $= Just keyboarder
  GLUT.closeCallback $= Just closer
  GLUT.fullScreen
  return ()

graphDeinit :: GraphicsState -> IO ()
graphDeinit st = do
  return ()
