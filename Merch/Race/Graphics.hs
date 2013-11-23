
module Merch.Race.Graphics
  ( DisplayFunc
  , displayFunc
  , MouseMoveFunc
  , mouseMoveFunc
  , KeyDownFunc
  , keyDownFunc
  , KeyUpFunc
  , keyUpFunc
  , initializeGraphics
  ) where

import Control.Exception
import Data.IORef
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.UI.GLUT.Callbacks.Window as GLUT
import Graphics.UI.GLUT(($=))
import System.IO.Unsafe

aspectvar :: IORef GL.GLdouble
{-# NOINLINE aspectvar #-}
aspectvar = unsafePerformIO $ newIORef 1.0

reshaper :: GLUT.ReshapeCallback
reshaper (GL.Size w h) = do
  GL.viewport $= (GLUT.Position 0 0, GL.Size w h)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  let aspect = fromIntegral w / fromIntegral h
  writeIORef aspectvar aspect
  if w >= h
    then GLU.ortho2D (-1 * aspect) (1 * aspect) (-1) 1
    else GLU.ortho2D (-1) 1 (-1 / aspect) (1 / aspect)
  GLUT.postRedisplay Nothing
closer :: GLUT.CloseCallback
closer = do
  GLUT.leaveMainLoop

type DisplayFunc = (GL.GLdouble -> GLUT.DisplayCallback)
{- Setup the display.  The given display function
   will be provided an aspect ratio.  The aspect >= 1
   if screen width is larger than screen height,
   aspect < 1 if screen width is less than screen
   height.  The projection matrix is set up
   so that (-1, -1) -> (1, 1) will always be displayed.
   If aspect > 1, then up to (-aspect, -1) to (aspect, 1)
   is displayable.  If aspect < 1, then up to
   (-1, -1/aspect) to (1, 1/aspect) is displayable.

   The display function only needs to send primitives
   and draws; setupDisplay will handle flushing to the
   screen.  -}
setupDisplay :: DisplayFunc -> IO ()
setupDisplay dispfunc = do
  let displayer = do
        GLUT.clear [GLUT.ColorBuffer]
        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity
        aspect <- readIORef aspectvar
        dispfunc aspect
        GLUT.flush
  GLUT.displayCallback $= displayer
  GL.get GLUT.windowSize >>= reshaper

displayFunc :: GL.SettableStateVar DisplayFunc
displayFunc = GL.makeSettableStateVar setupDisplay

type MouseMoveFunc = (GL.GLdouble, GL.GLdouble) -> IO ()
setupMouseMoveFunc :: MouseMoveFunc -> IO ()
setupMouseMoveFunc movefunc = do
  let mover pos = translateMouseToScreen pos >>= movefunc
  GLUT.motionCallback $= Just mover
  GLUT.passiveMotionCallback $= Just mover
mouseMoveFunc :: GL.SettableStateVar MouseMoveFunc
mouseMoveFunc = GL.makeSettableStateVar setupMouseMoveFunc

translateMouseToScreen :: GLUT.Position -> IO (GL.GLdouble, GL.GLdouble)
translateMouseToScreen (GLUT.Position xi yi) = do
  GL.Size wi hi <- GL.get GLUT.windowSize
  let (w, h) = (fromIntegral wi, fromIntegral hi)
      (x, y) = (fromIntegral xi, fromIntegral yi)
      smaller
        | wi < hi   = w
        | otherwise = h
      xv = ((2 * x) / smaller) - 1
      yv = ((2 * (h - y)) / smaller) - 1
  return (xv, yv)

type KeyDownFunc = Char -> IO ()
keyDownVar :: IORef KeyDownFunc
{-# NOINLINE keyDownVar #-}
keyDownVar = unsafePerformIO $ newIORef $ \c ->
  if c == '\ESC'
    then GLUT.leaveMainLoop
    else return ()
setupKeyDown :: KeyDownFunc -> IO ()
setupKeyDown = writeIORef keyDownVar
keyDownFunc :: GL.SettableStateVar KeyDownFunc
keyDownFunc = GL.makeSettableStateVar setupKeyDown

type KeyUpFunc = Char -> IO ()
keyUpVar :: IORef KeyUpFunc
{-# NOINLINE keyUpVar #-}
keyUpVar = unsafePerformIO $ newIORef $ \_ -> return ()
setupKeyUp :: KeyUpFunc -> IO ()
setupKeyUp = writeIORef keyUpVar
keyUpFunc :: GL.SettableStateVar KeyUpFunc
keyUpFunc = GL.makeSettableStateVar setupKeyUp

keyboarder :: GLUT.KeyboardMouseCallback
keyboarder (GLUT.Char c) (GLUT.Down) _ _ = readIORef keyDownVar >>= \f -> f c
keyboarder (GLUT.Char c) (GLUT.Up)   _ _ = readIORef keyUpVar >>= \f -> f c
keyboarder key updown modifiers pos      = return ()

initializeGraphics :: IO () -> IO ()
initializeGraphics act = bracket graphInit graphDeinit (\_ -> act)

graphInit :: IO ()
graphInit = do
  GLUT.initialize "Merchant's Race" []
  GLUT.createWindow "Merchant's Race"
  GLUT.displayCallback $= do
    GLUT.clear [GLUT.ColorBuffer]
    GLUT.flush
  GLUT.closeCallback $= Just closer
  GLUT.reshapeCallback $= Just reshaper
  GLUT.keyboardMouseCallback $= Just keyboarder
  GLUT.actionOnWindowClose $= GLUT.ContinueExectuion
  GLUT.fullScreen
  return ()
graphDeinit :: () -> IO ()
graphDeinit () = do
  return ()
