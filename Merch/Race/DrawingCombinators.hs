{- Wrapper for Graphics.DrawingCombinators.   -}

module Merch.Race.DrawingCombinators
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

  , Image
  , render
  , sample

  , point
  , line
  , regularPoly
  , circle
  , convexPoly
  , bezierCurve

  , Color(..)
  , modulate
  , tint

{-
  -- Graphics.DrawingCombinators' sprites leave much
  -- to be desired.  In particular, sprites are assumed
  -- to always have an aspect ratio of 1, or if not,
  -- for the caller to know beforehand what the aspect
  -- ratio is.
  , Sprite
  , openSprite
  , spriteAspect
  , sprite
-}

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

import Control.Applicative
import Graphics.DrawingCombinators

invisible :: Image a -> Image a
invisible im = unsafeOpenGLImage (\_ -> return ()) (sample im)

forceSample :: a -> Image b -> Image a
forceSample a im = pure (const a) <*> im

intersection :: Image Any -> Image Any -> Image Any
intersection a b = pure anyAnd <*> a <*> b
 where
  anyAnd (Any a) (Any b) = Any $ a && b
