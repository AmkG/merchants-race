{- Merch.Race.Top.MapGen - Map generator screen.

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
{- Map generator screen.  -}
module Merch.Race.Top.MapGen
  ( mapgenScreen
  ) where

import Merch.Race.Control.Background
import Merch.Race.Data.TMap
import Merch.Race.GameResources
import Merch.Race.Hex
import Merch.Race.MapGen
import Merch.Race.Ruleset
import Merch.Race.UI.Button
import Merch.Race.UI.Drawing
import Merch.Race.UI.DrawingCombinators
import Merch.Race.UI.Minimap

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import qualified Data.Set as Set
import System.Random

newtype MGX a
  = MGX {run :: ReaderT GameResources (BackM (String, Rational, MTMap)) a}
instance Monad MGX where
  return = MGX . return
  fail = MGX . fail
  ma >>= f = MGX $ run ma >>= run . f
instance MapGenM MGX where
  mgMapBounds = MGX $ do
    (s, p, m) <- get
    liftIO $ boundsMTMap m
  mgGetTerrain h = MGX $ do
    (s, p, m) <- get
    (t, _) <- liftIO $ readMTMap m h
    return t
  mgPutTerrain h t = MGX $ do
    (s, p, m) <- get
    (_, r) <- liftIO $ readMTMap m h
    liftIO $ writeMTMap m h (t, r)
    put (s, p, m)
  mgGetRoad h = MGX $ do
    (s, p, m) <- get
    (_, r) <- liftIO $ readMTMap m h
    return r
  mgPutRoad h r = MGX $ do
    (s, p, m) <- get
    (t, _) <- liftIO $ readMTMap m h
    liftIO $ writeMTMap m h (t, r)
    put (s, p, m)
  mgAddSettlement s st h = MGX $ do
    (step, p, m) <- get
    liftIO $ addSettlementMTMap m s st h
    put (step, p, m)
  mgPutDistance s1 s2 d = MGX $ do
    (step, p, m) <- get
    liftIO $ writeDistanceMTMap m s1 s2 d
    put (step, p, m)
  mgStep s = MGX $ do
    (_, p, m) <- get
    put (s, p, m)
  mgProgress p = MGX $ do
    (s, _, m) <- get
    put (s, p, m)
  mgRandom = MGX $ liftIO $ randomIO
  mgNameGenerator = MGX $ do
    gr <- ask
    return $ nameGenerator $ grRuleset gr
  mgSettlementGenerator = MGX $ do
    gr <- ask
    return $ settlementGenerator $ grRuleset gr
  mgRequiredTerrain st = MGX $ do
    gr <- ask
    return $ terrain (grRuleset gr) st

mapSize = (256, 256)

mapgenScreen :: GameResources -> (TMap -> Screen) -> Screen -> Screen
mapgenScreen gr onFinish onCancel _ _ = do
  mtm <- newMTMap (fromOffset (0, 0), fromOffset (fst mapSize - 1, snd mapSize - 1))
  let freeze (s,p,mtm) = do
        tm <- freezeMTMap mtm
        return (s,p,tm)
      startingState = ("Starting", 0, mtm)
  drawvar <- newIORef mempty

  bg <- runBackM freeze (runReaderT (run mapgen) gr) startingState
  return $ SetScreen $ runScreen gr onFinish onCancel bg drawvar
runScreen :: GameResources
          -> (TMap -> Screen) -- on success
          -> Screen -- on fail (user cancelled or aborted)
          -> Background (String, Rational, TMap) -- background map generation
          -> IORef Drawing -- variable used internally
          -> Screen -- screen to display
runScreen gr onFinish onCancel bg drawvar aspect = core
 where
  screenHeight
    | aspect < 1 = 1 / aspect
    | otherwise  = 1
  screenWidth
    | aspect > 1 = aspect
    | otherwise  = 1

  core ReDo = redraw
  core Idle = do
    mt <- completedBackground bg
    case mt of
      Just (_, _, tm) -> return $ SetScreen $ onFinish tm
      Nothing         -> do
        mt <- sampleBackground bg
        case mt of
          Just v  -> do
            updateDrawing v
            redraw
          Nothing -> return NoTopReaction
  core (KeyDown _ '\ESC') = return $ SetScreen $ abortScreen

  bc = mkButtonConfig
       [ ButtonFont $ grFont gr
       , ButtonWidth $ 0.8
       , ButtonHeight $ 0.12 * screenHeight
       , ButtonTextHeight $ 0.04 * screenHeight
       ]
  cancelButton =
    button bc (0, negate $ 0.9 * screenHeight) "Cancel" abortScreen

  redraw = do
    im <- readIORef drawvar
    return $ SetDrawing $ cancelButton `mappend` im

  progressHeight = 0.06 * screenHeight
  progressWidth = screenWidth
  progressY = negate $ 0.76 * screenHeight
  progressFontHeight = 0.04

  mapBottom = 0.7 * screenHeight
  mapHeight = (screenHeight + mapBottom) / 2
  mapMove = screenHeight - mapHeight

  updateDrawing (step, progress, tmap) = do
    let im = mconcat
             [ progressText
             , progressBar
             , forceSample (Any False) $ minimapImage
             ]
        minimapImage = translate (0, mapMove)
                    %% scale mapHeight mapHeight
                    %% minimap tmap Set.empty
        progressBar = tint (Color 0.1 1 0.1 1)
                    $ rectangle (lx, ly) (ux, uy)
         where
          lx = negate progressWidth
          ux = 2 * progressWidth * realToFrac progress - progressWidth
          ly = progressY - progressHeight
          uy = progressY + progressHeight
        progressText = mconcat
                       [ translate (x, y)
                       , scale adjust adjust
                       , translate (centering, negate 0.7)
                       ]
                    %% text (grFont gr) step
         where
          w = textWidth (grFont gr) step
          x = 0
          y = progressY - progressFontHeight * 0.4
          adjust = progressFontHeight
          centering = negate w / 2

    writeIORef drawvar $ drawingStatic im

  abortScreen _ _ = do
    abortBackground bg
    return $ SetScreen onCancel
