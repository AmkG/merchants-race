
{- Drawing system, based around graphics-drawingcombinators.  -}
module Merch.Race.Drawing
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

import qualified Merch.Race.DrawingCombinators as Draw

import Control.Applicative
import Control.Concurrent(yield)
import Control.Concurrent.MVar
import Control.Exception
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
              writeIORef screenvar s
              sz <- get GLUT.windowSize
              let aspect = computeAspect sz
                  top = s aspect
              writeIORef topvar top
              sendTop ReDo

    -- compute starting image
    sendTop ReDo

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

        -- TODO
        keyboarder (GLUT.Char '\ESC') GLUT.Down _ _ = GLUT.leaveMainLoop
        keyboarder k ks mod pos = do
          return ()

        -- TODO
        mover pos = do
          (x,y) <- translateMouseToScreen pos
          return ()

        idler = yield >> sendTop Idle

    GLUT.displayCallback $= displayer
    GLUT.closeCallback $= Just GLUT.leaveMainLoop
    GLUT.reshapeCallback $= Just reshaper
    GLUT.keyboardMouseCallback $= Just keyboarder
    GLUT.idleCallback $= Just idler
    GLUT.motionCallback $= Just mover
    GLUT.passiveMotionCallback $= Just mover
    GLUT.actionOnWindowClose $= GLUT.ContinueExectuion
    GLUT.mainLoop

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
