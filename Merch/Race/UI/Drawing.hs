{- Merch.Race.UI.Drawing - GUI/Screen framework.

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
{- Drawing system, based around graphics-drawingcombinators.  -}
module Merch.Race.UI.Drawing
  ( Drawing(..)
  , drawing
  , drawingStatic
  , Screen(..)
  , Reaction(..)
  , MouseEvent(..)
  , TopEvent(..)
  , Modifier(..)
  , TopReaction(..)

  , initialScreen
  ) where

import qualified Merch.Race.UI.DrawingCombinators as Draw

import Control.Applicative
import Control.Concurrent(yield, threadDelay)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Monoid
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=), get)
import System.IO.Unsafe

-- Responds to top-level events by maybe
-- replacing the drawing.
type Screen = Draw.R -> TopEvent -> IO TopReaction

newtype Drawing
  = Drawing (Draw.Image (Maybe (Int, MouseEvent -> [Reaction])))
instance Monoid Drawing where
  mempty = Drawing $ fmap (\ (Any _) -> Nothing) mempty
  mconcat [] = mempty
  mconcat (a:as) = foldl' mappend a as

  -- mappend on two drawings will change any
  -- Modify reactions to modify the append of
  -- both drawings.
  a `mappend` b = Drawing $ pure combine <*> ia <*> ib
   where
     (Drawing ia, Drawing ib) = (a, b)
     combine Nothing          Nothing          = Nothing
     combine (Just (ida, fa)) _                = Just (ida, map convert . fa)
      where
       convert (Modify a) = Modify $ a `mappend` b
       convert x          = x
     combine Nothing          (Just (idb, fb)) = Just (idb, map convert . fb)
      where
       convert (Modify b) = Modify $ a `mappend` b
       convert x          = x

-- Drawing-level events caused by mouse movement
data MouseEvent
  = MouseMove --     Mouse moved inside the drawing
  | MouseMoveOut --  Mouse moved outside the drawing
  | MouseDown --     Mouse button up->down event
  | MouseUp --       Mouse button down->up event
  deriving (Show, Read, Eq)

-- What a Drawing does in response to having mice
-- crawl over it.
data Reaction
  = Modify Drawing -- Modify the drawing
  | Replace Screen -- Completely replace the screen
  | Grab --           Grab mouse handling
  | Ungrab --         Ungrab mouse handling

-- Top-level events, handled by Screen functions.
data TopEvent
  = ReDo --                                Completely redraw.
  | Idle --                                Idle event.
  | KeyDown [Modifier] Char --             Keyboard event.
  | KeyUp   [Modifier] Char --             Keyboard event.
  | SKeyDown [Modifier] GLUT.SpecialKey -- Non-character keypress.
  | SKeyUp   [Modifier] GLUT.SpecialKey -- Non-character keypress.
  deriving (Eq, Ord, Show)
data Modifier
  = Ctrl
  | Alt
  | Shift
  deriving (Show, Read, Eq, Ord)
data TopReaction
  = NoTopReaction --      Do nothing.
  | SetDrawing Drawing -- Set the drawing.
  | SetScreen Screen --   Modify the screen; new screen receives ReDo.

drawingIdVar :: MVar Int
{-# NOINLINE drawingIdVar#-}
drawingIdVar = unsafePerformIO $ newMVar 0

drawing :: (MouseEvent -> [Reaction]) -> Draw.Image Any -> Drawing
{-# NOINLINE drawing #-}
drawing r i = unsafePerformIO $ do
  id <- takeMVar drawingIdVar
  putMVar drawingIdVar $ id + 1
  return $ Drawing $ fmap (install r id) i
 where
  install r id (Any False) = Nothing
  install r id (Any True)  = Just (id, r)

drawingStatic :: Draw.Image a -> Drawing
drawingStatic = Drawing . Draw.forceSample Nothing

-- Core initialization, also enters main loop.
initialScreen :: Screen -> IO ()
initialScreen startscreen = bracket graphInit graphDeinit $ \_ -> core
 where
  core = do

    screenvar <- newIORef startscreen

    -- compute aspect ratio
    startsz <- get GLUT.windowSize
    let startaspect = computeAspect startsz
        starttop = startscreen startaspect
    topvar <- newIORef starttop

    -- set up image variable
    dispvar <- newIORef mempty

    let sendTop msg = do
          case msg of
            ReDo -> writeIORef dispvar mempty
            _    -> return ()
          top <- readIORef topvar
          result <- top msg
          case result of
            NoTopReaction -> return ()
            SetDrawing d  -> do
              writeIORef dispvar d
              GLUT.postRedisplay Nothing
            SetScreen  s  -> do
              setScreen s
        setScreen s = do
          writeIORef screenvar s
          sz <- get GLUT.windowSize
          let aspect = computeAspect sz
              top = s aspect
          writeIORef topvar top
          sendTop ReDo

    -- compute starting image
    sendTop ReDo

    -- set up variables for mouse movement
    -- current item and handler that mouse is on.
    curvar <- newIORef Nothing
    -- the mouse location when the mouse was grabbed.
    grabvar <- newIORef Nothing

    -- function definitions
    let displayer = do
          GLUT.clear [GLUT.ColorBuffer]
          GL.matrixMode GL.$= GL.Modelview 0
          GL.loadIdentity
          Drawing im <- readIORef dispvar
          Draw.render im
          GLUT.flush

        reshaper (GLUT.Size w h) = do
          let aspect = computeAspect $ GLUT.Size w h
              glaspect = realToFrac aspect
          -- update projection
          GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
          GL.matrixMode GL.$= GL.Projection
          GL.loadIdentity
          if w >= h
           then GLU.ortho2D (-1 * glaspect) (1 * glaspect) (-1) 1
           else GLU.ortho2D (-1) 1 (-1 / glaspect) (1 / glaspect)
          -- update screen
          s <- readIORef screenvar
          let top = s aspect
          writeIORef topvar top
          sendTop ReDo

        keyboarder (GLUT.Char c)       GLUT.Down mod _
          = sendTop $ KeyDown  (glutModToModifier mod) c
        keyboarder (GLUT.Char c)       GLUT.Up   mod _
          = sendTop $ KeyUp    (glutModToModifier mod) c
        keyboarder (GLUT.SpecialKey c) GLUT.Down mod _
          = sendTop $ SKeyDown (glutModToModifier mod) c
        keyboarder (GLUT.SpecialKey c) GLUT.Up   mod _
          = sendTop $ SKeyUp   (glutModToModifier mod) c
        keyboarder (GLUT.MouseButton GLUT.LeftButton) GLUT.Up   _ pos
          = mouseButtonSend pos MouseUp
        keyboarder (GLUT.MouseButton GLUT.LeftButton) GLUT.Down _ pos
          = mouseButtonSend pos MouseDown
        keyboarder k ks mod pos = do
          return ()

        mouseButtonSend :: GLUT.Position -> MouseEvent -> IO ()
        mouseButtonSend pos msg = do
          (x,y) <- translateMouseToScreen pos
          moverLoop (x, y)
          cur <- readIORef curvar
          case cur of
            Just (_, curfun) -> do
              handleReaction curfun msg (x,y)
            Nothing          -> return ()

        mover pos = translateMouseToScreen pos >>= moverLoop
        moverLoop (x,y) = do
          trygrab <- readIORef grabvar
          case trygrab of
            Just (gx,gy) -> do
              Drawing im <- readIORef dispvar
              case Draw.sample im (gx,gy) of
                Nothing -> writeIORef grabvar Nothing
                _       -> return ()
            Nothing      -> return ()

          grab <- readIORef grabvar
          case grab of
           -- currently grabbed
           Just (gx, gy) -> do
            Drawing im <- readIORef dispvar
            let Just (curid, curfun) = Draw.sample im (gx,gy)
                actualCurItem = Draw.sample im (x, y)
                isOut = case actualCurItem of
                  Nothing      -> True
                  Just (id, _) -> id /= curid
                msg
                  | isOut     = MouseMoveOut
                  | otherwise = MouseMove
                fun = case actualCurItem of
                  Nothing          -> curfun
                  Just (_, actfun)
                    | isOut        -> curfun
                    | otherwise    -> actfun
            writeIORef curvar $ Just (curid, fun)
            handleReaction fun msg (x,y)
            -- got ungrabbed?  Resend message.
            whenNotGrab $ do
              curvarUpdate(x,y)
              moverLoop (x,y)

           -- currently ungrabbed
           Nothing -> do
            cur <- readIORef curvar
            Drawing im <- readIORef dispvar
            let act = Draw.sample im (x, y)
            case cur of
              Just (curid, curfun) -> do
                case act of
                  Just (actid, actfun) -> do
                    -- both a previous and a current exist.
                    if curid /= actid
                     then do
                      handleReaction curfun MouseMoveOut (x,y)
                      whenNotGrab $ do
                        -- pass it to current handler
                        writeIORef curvar Nothing
                        moverLoop (x,y)
                     else do
                      handleReaction curfun MouseMove (x,y)
                      curvarUpdate(x,y)
                  Nothing -> do
                    -- a previous exists but not a current.
                    handleReaction curfun MouseMoveOut (x,y)
                    -- retry grab
                    whenNotGrab $ do
                      -- pass it to current handler
                      writeIORef curvar Nothing
                      moverLoop (x, y)
              Nothing -> do
                case act of
                  Just (actid, actfun) -> do
                    -- some item contains it.
                    handleReaction actfun MouseMove (x,y)
                    curvarUpdate(x,y)
                  -- nothing in particular
                  Nothing -> return ()

        handleReaction rf msg pos = forM_ (rf msg) (handle1 msg pos)
        handle1 msg (x,y) Grab       = writeIORef grabvar $ Just (x,y)
        handle1 msg (x,y) Ungrab     = writeIORef grabvar Nothing
        handle1 msg (x,y) (Modify d) = do
          writeIORef dispvar d
          GLUT.postRedisplay Nothing
        handle1 msg (x,y) (Replace s) = do
          writeIORef curvar Nothing
          writeIORef grabvar Nothing
          setScreen s

        curvarUpdate(x,y) = do
          Drawing im <- readIORef dispvar
          writeIORef curvar $ Draw.sample im (x,y)
        whenNotGrab act = do
          grab <- readIORef grabvar
          case grab of
            Nothing -> act
            _       -> return ()

        idler = threadDelay 1 >> yield >> sendTop Idle

    GLUT.displayCallback $= displayer
    GLUT.closeCallback $= Just GLUT.leaveMainLoop
    GLUT.reshapeCallback $= Just reshaper
    GLUT.keyboardMouseCallback $= Just keyboarder
    GLUT.idleCallback $= Just idler
    GLUT.motionCallback $= Just mover
    GLUT.passiveMotionCallback $= Just mover
    GLUT.actionOnWindowClose $= GLUT.ContinueExectuion
    GLUT.mainLoop

glutModToModifier :: GLUT.Modifiers -> [Modifier]
glutModToModifier m
  = [Ctrl  | GLUT.Down <- return $ GLUT.ctrl  m]
 ++ [Shift | GLUT.Down <- return $ GLUT.shift m]
 ++ [Alt   | GLUT.Down <- return $ GLUT.alt   m]

computeAspect :: GLUT.Size -> Draw.R
computeAspect (GLUT.Size w h) = fromIntegral w / fromIntegral h

translateMouseToScreen :: GLUT.Position -> IO (Draw.R, Draw.R)
translateMouseToScreen (GLUT.Position xi yi) = do
  GLUT.Size wi hi <- GLUT.get GLUT.windowSize
  let (w, h) = (fromIntegral wi, fromIntegral hi)
      (x, y) = (fromIntegral xi, fromIntegral yi)
      smaller
        | wi < hi   = w
        | otherwise = h
      (xadj, yadj)
        | wi < hi   = (1    , h / w)
        | otherwise = (w / h, 1    )
      xv = ((2 * x) / smaller) - xadj
      yv = ((2 * (h - y)) / smaller) - yadj
  return (xv, yv)

graphInit :: IO ()
graphInit = do
  GLUT.initialize "Merchant's Race" []
  GLUT.createWindow "Merchant's Race"
  GLUT.fullScreen
  return ()
graphDeinit :: () -> IO ()
graphDeinit () = do
  return ()
