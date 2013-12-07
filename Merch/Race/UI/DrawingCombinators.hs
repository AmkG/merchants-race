{- Merch.Race.UI.DrawingCombinators - Wrapper for Graphics.DrawingCombinators

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

{- Wrapper for Graphics.DrawingCombinators.   -}

module Merch.Race.UI.DrawingCombinators
  ( Affine
  , R
  , R2
  , compose
  , apply
  , identity
  , translate
  , rotate
  , scale
  , inverse
  , (%%)

  , Image
  , render
  , sample

  , point
  , line
  , regularPoly
  , circle
  , convexPoly
  , bezierCurve
  , rectangle -- create a rectangle

  , Color(..)
  , modulate
  , tint

  -- Graphics.DrawingCombinators' sprites leave much
  -- to be desired.  In particular, sprites are assumed
  -- to always have an aspect ratio of 1, or if not,
  -- for the caller to know beforehand what the aspect
  -- ratio is.
  , Sprite
  , openSprites
  , openSprite
  , spriteAspect
  , sprite

  , Font
  , openFont
  , text
  , textWidth

  -- Special operations
  , invisible -- Don't render the image, but sample still works
  , forceSample -- Force an image to have a particular sample on all points
  , intersection -- Combine the samples of two (Image Any) using an
  --                intersection instead of union (default in Monoid of Any).

  , Monoid(..)
  , Any(..)
  ) where

import qualified Codec.Image.STB as STB
import Control.Applicative
import Control.Monad
import qualified Data.Bitmap.Base as BMP
import Data.List
import Graphics.DrawingCombinators hiding (Sprite, sprite, openSprite)
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL(($=))
import System.IO.Unsafe

invisible :: Image a -> Image a
invisible im = unsafeOpenGLImage (\_ -> return ()) (sample im)

forceSample :: a -> Image b -> Image a
forceSample a im = pure (const a) <*> im

intersection :: Image Any -> Image Any -> Image Any
intersection a b = pure anyAnd <*> a <*> b
 where
  anyAnd (Any a) (Any b) = Any $ a && b

{- Sprite reimplementation.  -}
data Sprite
  = Sprite
    { sTexture :: GL.TextureObject
    -- texture coordinates.
    , sLowerLeft :: (GL.GLdouble, GL.GLdouble)
    , sUpperRight :: (GL.GLdouble, GL.GLdouble)
    -- aspect ratio
    , spriteAspect :: GL.GLdouble
    }
openSprites :: [FilePath] -> IO [Sprite]
openSprites [] = return []
openSprites fs = do
  let -- We will be sorting the sprite bitmaps
      -- in order from largest to smallest,
      -- so add an ordering id to ensure that
      -- our outputs will be ordered the same
      -- as the inputs.
      ifs = zip [0..] fs :: [(Integer, FilePath)]
  ibss <- forM ifs $ \ (i, f) -> do
    eb <- STB.loadImage f
    case eb of
      Left e  -> fail e
      Right b -> return (i, b, BMP.bitmapSize b)
  -- put the widest items first.
  let sorted_ibss = sortBy (\ (_,_,a) (_,_,b) -> compare a b) ibss
  (tex, itxlas) <- layout sorted_ibss
  let iss :: [(Integer, Sprite)]
      iss = map (\ (i, ll, ur, a) -> (i, Sprite tex ll ur a)) itxlas
      sorted_iss = sortBy (\ (a, _) (b, _) -> compare a b) iss
  return $ map snd sorted_iss
 where
  -- Degenerate case
  layout [(i, b, s)] = do
    let aspect = getAspect s
        (w, h) = s
        larger
          | w > h     = w
          | otherwise = h
        texDim = pow2 larger
    undefined
  layout (ibs:ibss)  = do
    undefined
  -- Compute the aspect ratio
  getAspect (w, h) = fromIntegral w / fromIntegral h :: GL.GLdouble
  -- Find the next higher power of 2
  pow2 = loop 1
   where
    loop l i
      | l > i     = l
      | otherwise = loop (l * 2) i
openSprite :: FilePath -> IO Sprite
openSprite f = do
  [s] <- openSprites [f]
  return s
sprite :: Sprite -> Image Any
sprite s = unsafeOpenGLImage render pick
 where
  thisTexture = sTexture s
  render _ = do
    cur <- GL.get $ GL.textureBinding GL.Texture2D
    change <- case cur of
      Nothing -> return True
      Just t  -> return $ thisTexture /= t
    when change $ do
      GL.texture GL.Texture2D $= GL.Enabled
      GL.textureBinding GL.Texture2D $= Just thisTexture
    GL.renderPrimitive GL.Quads $ do
      let (lox, loy) = sLowerLeft s
          (hix, hiy) = sUpperRight s
      texcoord lox loy
      vertex (negate xr) (yr)
      texcoord hix loy
      vertex (xr)        (yr)
      texcoord hix hiy
      vertex (xr)        (negate yr)
      texcoord lox hiy
      vertex (negate xr) (negate yr)
  texcoord x y = GL.texCoord $ GL.TexCoord2 x y
  vertex x y = GL.vertex $ GL.Vertex2
                 (realToFrac x :: GL.GLdouble)
                 (realToFrac y :: GL.GLdouble)
  aspect = realToFrac $ spriteAspect s
  (xr, yr)
    | aspect >= 1  = (aspect, 1         )
    | otherwise    = (1     , 1 / aspect)
  pick (x,y)
    | negate xr <= x && x <= xr && negate yr <= y && y <= yr = Any True
    | otherwise                                              = Any False

-------

rectangle :: (R, R) -> (R, R) -> Image Any
rectangle (x1, y1) (x2, y2)
  = core (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)
 where
  core lx ly ux uy
    = convexPoly
      [ (lx, ly)
      , (ux, ly)
      , (ux, uy)
      , (lx, uy)
      ]
