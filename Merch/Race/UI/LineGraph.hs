{- Merch.Race.UI.LineGraph - Line graph drawing implementation.

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

{- Implements drawings of line graphs.  -}

module Merch.Race.UI.LineGraph
  ( lineGraph
  ) where

import Merch.Race.UI.DrawingCombinators

import Control.Concurrent.MVar
import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Graphics.DrawingCombinators(unsafeOpenGLImage)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL(($=))
import System.IO.Unsafe

{- Draw several line graphs together.  All samples
   must be positive.  Each line graph must have at
   least two samples.  -}
lineGraph :: RealFrac d => R -> [[d]] -> Image ()
lineGraph lineWidth ass = unsafeOpenGLImage render (const ())
 where
  halfWidth = realToFrac $ lineWidth / 2

  highest = realToFrac $ foldl' max (negate 10000) $ concat ass
  step = 100
  graphTop = step * fromIntegral (ceiling (highest / step))
  graphYscale = 1 / graphTop

  samples = foldl' min (10000) $ map length ass
  graphXscale = 1 / (fromIntegral $ samples - 1)

  numVertex = samples * 2
  vboSize = fromIntegral $ numVertex * 2 * (sizeOf (0 :: GL.GLfloat))

  render c = do
    vbovar <- getvbovar samples
    withMVar vbovar $ \vbo -> do
      GL.bindBuffer GL.ArrayBuffer $= Just vbo
      GL.clientState GL.VertexArray $= GL.Enabled
      forM_ ass $ \ as -> do
        -- clear the buffer
        GL.bufferData GL.ArrayBuffer $= (vboSize, nullPtr, GL.StreamDraw)
        -- fill it
        let filler p = do
              forM_ (zip [0..(samples-1)] as) $ \ (i,a) -> do
                let x,y :: GL.GLfloat
                    x = 2 * fromIntegral i * graphXscale - 1
                    y = 2 * realToFrac a * graphYscale - 1
                pokeElemOff p (i * 4 + 0) x
                pokeElemOff p (i * 4 + 1) (y + halfWidth)
                pokeElemOff p (i * 4 + 2) x
                pokeElemOff p (i * 4 + 3) (y - halfWidth)
            errorer = fail . show
        GL.withMappedBuffer GL.ArrayBuffer GL.WriteOnly filler errorer
        -- Tell OpenGL to load vertices from the buffer
        GL.arrayPointer GL.VertexArray $= GL.VertexArrayDescriptor
          2 GL.Float (fromIntegral $ 2 * sizeOf (0 :: GL.GLfloat)) nullPtr
        -- DRAW!
        GL.drawArrays GL.TriangleFan 0 (fromIntegral numVertex)

      GL.clientState GL.VertexArray $= GL.Disabled
      GL.bindBuffer GL.ArrayBuffer $= Nothing

{- The stream-in VBO's.  -}
vbomapvar :: MVar (Map Int (MVar GL.BufferObject))
{-# NOINLINE vbomapvar #-}
vbomapvar = unsafePerformIO $ newMVar Map.empty

getvbovar :: Int -> IO (MVar GL.BufferObject)
getvbovar s = modifyMVar vbomapvar $ \vbomap -> do
  case Map.lookup s vbomap of
    Nothing -> do
      -- create the VBO, the variable, and insert into
      -- the map.
      [vbo] <- GL.genObjectNames 1
      vbovar <- newMVar vbo
      let nvbomap = Map.insert s vbovar vbomap
      return (nvbomap, vbovar)
    Just vbovar -> do
      return (vbomap, vbovar)

