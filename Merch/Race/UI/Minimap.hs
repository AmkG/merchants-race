{- Merch.Race.UI.Minimap - Draws a minimap for a TMap.

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
{- Renders a minimap for the given TMap.  -}
module Merch.Race.UI.Minimap
  ( minimap

  , minimapCore
  , MinimapPrepare
  , minimapPrepare
  ) where

import Merch.Race.Data
import Merch.Race.Data.TMap
import qualified Merch.Race.Hex as H
import Merch.Race.Hex hiding(position)
import Merch.Race.UI.DrawingCombinators

import Control.Concurrent.MVar
import Control.Monad
import Data.Ix
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Monoid
import qualified Data.Set as Set
import Data.Set(Set)
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL(($=))
import Graphics.DrawingCombinators(unsafeOpenGLImage)

import System.IO.Unsafe

{- The minimap is scaled for -1,-1 to 1,1.
   Also, the minimap renders hex coordinates
   positive right and downward, and outputs
   OpenGL coordinates that go positive
   right and upward.

   The parameter hidden is the set of hidden
   tiles (i.e. tiles not yet explored by the
   merchant).  -}

minimap :: TMap -> Set HexCoord -> Image (First HexCoord)
minimap tm hidden = minimapCore (findPrepared $ boundsTMap tm) tm hidden

findPrepared :: (HexCoord, HexCoord) -> MinimapPrepare
{-# NOINLINE findPrepared #-}
findPrepared bounds = unsafePerformIO $ do
  modifyMVar mvPreparedCache $ \mp -> do
    case Map.lookup bounds mp of
      Just a -> return (mp, a)
      Nothing -> do
        a <- minimapPrepare bounds
        let mp' = Map.insert bounds a mp
        return (mp', a)
mvPreparedCache :: MVar (Map (HexCoord, HexCoord) MinimapPrepare)
{-# NOINLINE mvPreparedCache #-}
mvPreparedCache = unsafePerformIO $ newMVar Map.empty

minimapCore :: MinimapPrepare -> TMap -> Set HexCoord -> Image (First HexCoord)
minimapCore pre tm hidden = adjustment %% total
 where
  (lb, ub) = boundsTMap tm
  superlb = fromOffset $ (\ (q,r) -> (q-1,r-1)) $ toOffset lb
  superub = fromOffset $ (\ (q,r) -> (q+1,r+1)) $ toOffset ub
  -- Note: the superlb (0,0) has the lower x coordinate, but
  -- the higher y coordinate, due to fact that in OpenGL,
  -- y goes positive upwards, while in our Hex coordinate
  -- system, it goes positive downwards.
  (lowx, highy)  = position superlb
  (highx, lowy)  = position superub
  adjustx = 2 / (highx - lowx)
  adjusty = 2 / (highy - lowy)
  supercenter = (negate $ (lowx + highx) / 2, negate $ (lowy + highy) / 2)
  adjustment = scale adjustx adjusty `mappend` translate supercenter

  backgroundColor = Color 0 0 0 1
  settlementColor = Color 1.0 0.0 0.0  1
  roadColor = Color 0.0 0.0 0   1
  terrainColor Sea        = Color 0.1 0.1 0.9  1
  terrainColor Freshwater = Color 0.3 0.4 1.0  1
  terrainColor Coast      = Color 0.8 0.9 0.4  1
  terrainColor Plains     = Color 0.3 0.9 0.3  1
  terrainColor Forest     = Color 0.0 0.8 0.0  1
  terrainColor Hill       = Color 0.5 0.5 0.1  1
  terrainColor Mountain   = Color 0.6 0.6 0.6  1

  background = tint backgroundColor $ rectangle (lowx, lowy) (highx, highy)

  total = mconcat
          [ settlements
          , roads
          , terrains
          ]
  settlements = tint settlementColor
              $ mconcat
              $ map (forceSample (First Nothing) . mkSettlement)
                    (settlementsTMap tm)
  mkSettlement (_,_,h) = (translate (x,y) `compose` scale 2 2)
                      %% regularPoly 4
    where
      (x,y) = position h
  -- TODO
  roads = tint roadColor
        $ mconcat
        $ concatMap mkRoad
                    (filter chooseRoad (range (lb,ub)))
  chooseRoad h = not (Set.member h hidden)
              && snd (lookupTMap tm h)
  mkRoad h = map (forceSample (First Nothing) . core)
                 (filter (snd . lookupTMap tm)
                         (neighbors h))
   where
    ph = position h
    core h2 = line ph $ halfway ph (position h2)

  -- Drawn tile hexes.
  terrains
    | mpBounds pre /= boundsTMap tm =
      error "Merch.Race.UI.Minimap.minimapCore: Incorrect preparation."
    | otherwise                     =
      unsafeOpenGLImage terrainDraw terrainPick
  terrainDraw tintC = do
    -- Generate normalized colors
    let n = normalizeColor
        seaColor        = n $ tintC `modulate` terrainColor Sea
        freshwaterColor = n $ tintC `modulate` terrainColor Freshwater
        coastColor      = n $ tintC `modulate` terrainColor Coast
        plainsColor     = n $ tintC `modulate` terrainColor Plains
        forestColor     = n $ tintC `modulate` terrainColor Forest
        hillColor       = n $ tintC `modulate` terrainColor Hill
        mountainColor   = n $ tintC `modulate` terrainColor Mountain
        nTerrainColor Sea        = seaColor
        nTerrainColor Freshwater = freshwaterColor
        nTerrainColor Coast      = coastColor
        nTerrainColor Plains     = plainsColor
        nTerrainColor Forest     = forestColor
        nTerrainColor Hill       = hillColor
        nTerrainColor Mountain   = mountainColor
        
    -- Grab the color buffer object
    withMVar (mpColorData pre) $ \colorBO -> do
      -- prepare a new buffer.
      GL.bindBuffer GL.ArrayBuffer $= Just colorBO
      GL.bufferData GL.ArrayBuffer $= (mpColorSize pre, nullPtr, GL.StreamDraw)
      -- fill the color buffer object.
      let filler p = do
            forM_ (zip [0..] (terrainsTMap tm)) $ \ (i, (t, _)) -> do
              let (r,g,b,a) = nTerrainColor t
              forM_ ([0..5]) $ \ n -> do
                 pokeElemOff p (i * 24 + n * 4 + 0) r
                 pokeElemOff p (i * 24 + n * 4 + 1) g
                 pokeElemOff p (i * 24 + n * 4 + 2) b
                 pokeElemOff p (i * 24 + n * 4 + 3) a
          errorer = fail . show
      GL.withMappedBuffer GL.ArrayBuffer GL.WriteOnly filler errorer
      -- Set up data
      GL.arrayPointer GL.ColorArray $= GL.VertexArrayDescriptor
        {-rgba-} 4 GL.UnsignedByte (fromIntegral $ 4 * sizeOf (0 :: GL.GLubyte)) nullPtr

      -- Enable arrays
      GL.clientState GL.VertexArray $= GL.Enabled
      GL.clientState GL.ColorArray $= GL.Enabled

      -- Bind vertices
      GL.bindBuffer GL.ArrayBuffer $= Just (mpHexVertices pre)
      GL.arrayPointer GL.VertexArray $= GL.VertexArrayDescriptor
        {-x,y-} 2 GL.Float (fromIntegral $ 2 * sizeOf (0 :: GL.GLfloat)) nullPtr

      -- Bind element index
      GL.bindBuffer GL.ElementArrayBuffer $= Just (mpHexElements pre)

      -- DRAW!
      GL.drawElements GL.Triangles (3 * mpNumElements pre) GL.UnsignedInt nullPtr

      -- Clean up
      GL.clientState GL.VertexArray $= GL.Disabled
      GL.clientState GL.ColorArray $= GL.Disabled
      GL.bindBuffer GL.ElementArrayBuffer $= Nothing
  terrainPick pt
    | inRange (lb,ub) h = First $ Just h
    | otherwise         = First Nothing
   where
    h = fromPosition pt

-- Normalizes a color to 4 byte representation
normalizeColor :: Color -> (GL.GLubyte, GL.GLubyte, GL.GLubyte, GL.GLubyte)
normalizeColor (Color r g b a) =
  (round $ 255 * r, round $ 255 * g, round $ 255 * b, round $ 255 * a)

data MinimapPrepare
  = MP
    { mpBounds :: (HexCoord, HexCoord) -- Bounds of the TMap this prepared minimap can handle
    , mpHexVertices :: GL.BufferObject -- The vertex buffer object for hexes of this minimap
    , mpHexElements :: GL.BufferObject -- The indexes to draw for hexes
    , mpNumElements :: GL.GLsizei --      Number of triangles to draw for all hexes
    , mpColorData :: MVar GL.BufferObject -- GPU memory for colordata
    , mpColorSize :: GL.GLsizeiptr --     The number of bytes of color data.
    }
{- Prepare the minimap data.  -}
minimapPrepare :: (HexCoord, HexCoord) -> IO MinimapPrepare
minimapPrepare bounds = do

  -- Initialize vertices.
  let numHexes = rangeSize bounds
      -- The size of the GLfloat coordinates to allocate.
      sizeVertices = numHexes * 6 * 2 * sizeOf (0 :: GL.GLfloat)
  -- Create the vertex buffer object.
  vertexBO <- allocaBytes sizeVertices $ \p -> do
    -- load the array.
    forM_ (zip [0..] (range bounds)) $ \ (i, h) -> do
      let c0 = position h
          [c1, c2, c3, c4, c5, c6] = map position $ neighbors h
          ps = [ center c0 c1 c2
               , center c0 c2 c3
               , center c0 c3 c4
               , center c0 c4 c5
               , center c0 c5 c6
               , center c0 c6 c1
               ] :: [(GL.GLfloat, GL.GLfloat)]
      forM_ (zip [0..] ps) $ \ (n, (x, y)) -> do
        pokeElemOff p (i * 12 + n * 2 + 0) x
        pokeElemOff p (i * 12 + n * 2 + 1) y
    -- load into GPU.
    [vertexBO] <- GL.genObjectNames 1 :: IO [GL.BufferObject]
    GL.bindBuffer GL.ArrayBuffer $= Just vertexBO
    GL.bufferData GL.ArrayBuffer $= (fromIntegral sizeVertices, p, GL.StaticDraw)
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    return vertexBO

  -- Initialize element indexes.
  let -- The size of the GLuint indexes to allocate.
      numElements = numHexes * 4
      sizeIndexes = numElements * 3 * sizeOf (0 :: GL.GLuint)
      -- Each hex is composed of 4 triangles, like so:
      {-2 _____ 1
         /|   /|\
      3 / |  / | \ 0
        \ | /  | /
       4 \|/___|/ 5
      -}
      -- We use the Triangles element mode, so that we
      -- can render the entire tileset in a single
      -- GL.drawElements call.
  -- create the buffer object.
  indexBO <- allocaBytes sizeIndexes $ \p -> do
    -- load the array.
    forM_ (zip [0..] (range bounds)) $ \ (i, _) -> do
      let tsRaw = [(0,1,5), (1,2,4), (2,3,4), (1,4,5)]
               :: [(GL.GLuint, GL.GLuint, GL.GLuint)]
          off = fromIntegral i * 6
          ts = map (\ (a,b,c) -> (off + a, off + b, off + c) ) tsRaw
      forM_ (zip [0..] ts) $ \ (n, (a,b,c)) -> do
        pokeElemOff p (i * 12 + n * 3 + 0) a
        pokeElemOff p (i * 12 + n * 3 + 1) b
        pokeElemOff p (i * 12 + n * 3 + 2) c
    -- load into GPU.
    [indexBO] <- GL.genObjectNames 1 :: IO [GL.BufferObject]
    GL.bindBuffer GL.ElementArrayBuffer $= Just indexBO
    GL.bufferData GL.ElementArrayBuffer $= (fromIntegral sizeIndexes, p, GL.StaticDraw)
    GL.bindBuffer GL.ElementArrayBuffer $= Nothing
    return indexBO

  -- Allocate GPU memory for colors.
  let -- Each hex has a single color, so need to write
      -- that color to the six corresponding vertices.
      numColors = numHexes * 6
      sizeColor = numColors * 4 * sizeOf (0 :: GL.GLubyte)
  [colorBO] <-  GL.genObjectNames 1 :: IO [GL.BufferObject]
  mvColorData <- newMVar colorBO

  return $ MP
           { mpBounds = bounds
           , mpHexVertices = vertexBO
           , mpHexElements = indexBO
           , mpNumElements = fromIntegral numElements
           , mpColorData = mvColorData
           , mpColorSize = fromIntegral $ sizeColor
           }

-- Get the center of three points
center :: Fractional r => (r,r) -> (r,r) -> (r,r) -> (r,r)
center (x1, y1) (x2, y2) (x3, y3) =
  ((x1 + x2 + x3) / 3, (y1 + y2 + y3) / 3)

-- Get the center of two points
halfway :: Fractional r => (r,r) -> (r,r) -> (r,r)
halfway (x1, y1) (x2, y2) =
  ((x1 + x2) / 2, (y1 + y2) / 2)

-- Our adjusted position, to handle our Hex coordinates
-- having the opposite y direction to the OpenGL coordinates.
-- We just use the expedient of negating the Y direction.
position :: Fractional r => HexCoord -> (r,r)
position h = let (x,y) = H.position h in (x, negate y)

glPrintError :: String -> IO ()
glPrintError s = do
  putStrLn $ "GL error at " ++ s
  GL.get GL.errors >>= mapM_ (putStrLn . show)
