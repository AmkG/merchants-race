
{- General button creation.  -}
module Merch.Race.UI.Button
  ( buttonGeneric
  , button
  , ButtonConfig
  , ButtonConfigOpt(..)
  , mkButtonConfig
  ) where

import Merch.Race.UI.Drawing
import qualified Merch.Race.UI.DrawingCombinators as Draw
import Merch.Race.UI.DrawingCombinators((%%))

import Data.List
import Data.Monoid

buttonGeneric :: Draw.Image Any -> Draw.Image Any -> Draw.Image Any
                 -> Screen -> Drawing
buttonGeneric idle hover press screen = idleState
 where
  idleF MouseMove = [Modify hoverState]
  idleF _         = []
  idleState = drawing idleF idle

  hoverF MouseMoveOut = [Modify idleState]
  hoverF MouseDown    = [Grab, Modify pressState]
  hoverF _            = []
  hoverState = drawing hoverF hover

  pressF MouseMoveOut = [Modify pressOffState]
  pressF MouseUp      = [Ungrab, Replace screen]
  pressF _            = []
  pressState = drawing pressF press

  pressOffF MouseMove = [Modify pressState]
  pressOffF MouseUp   = [Ungrab, Modify idleState]
  pressOffF _         = []
  pressOffState = drawing pressOffF idle

data ButtonConfig
  = BC
    { bcFont :: Draw.Font
    , bcWidth :: Draw.R
    , bcHeight :: Draw.R
    , bcTextHeight :: Draw.R
    }
data ButtonConfigOpt
  = ButtonFont Draw.Font
  | ButtonWidth Draw.R
  | ButtonHeight Draw.R
  | ButtonTextHeight Draw.R

mkButtonConfig :: [ButtonConfigOpt] -> ButtonConfig
mkButtonConfig = foldl' process start
 where
  start = BC
          { bcFont = undefined
          , bcWidth = 0.8
          , bcHeight = 0.12
          , bcTextHeight = 0.05
          }
  process c (ButtonFont f)       = c { bcFont = f }
  process c (ButtonWidth w)      = c { bcWidth = w }
  process c (ButtonHeight h)     = c { bcHeight = h }
  process c (ButtonTextHeight t) = c { bcTextHeight = t }

button :: ButtonConfig -> (Draw.R, Draw.R) -> String -> Screen -> Drawing
button bc (x,y) l screen = buttonGeneric idle hover press screen
 where
  BC font fw fh t = bc

  labelRaw = Draw.text font l
  labelCentered
    = Draw.translate (negate $ Draw.textWidth font l / 2, negate 0.7) %% labelRaw
  labelSized
    = Draw.scale t t %% labelCentered
  labelMoved
    = Draw.forceSample (Any False) $ Draw.translate (x,y) %% labelSized

  w = fw / 2; nw = negate w
  h = fh / 2; nh = negate h

  -- TODO: use curve-edge rectangles
  innerBox = Draw.rectangle (nw+0.01, nh+0.01) (w-0.01, h-0.01)
  outerBox = Draw.rectangle (nw     , nh     ) (w     , h     )

  innerBoxMoved
    = Draw.translate (x,y) %% innerBox
  outerBoxMoved
    = Draw.translate (x,y) %% outerBox

  white = Draw.Color 1   1   1   1
  gray  = Draw.Color 0.3 0.3 0.3 1
  black = Draw.Color 0   0   0   1

  idle
    = mconcat
      [ Draw.tint white labelMoved
      , Draw.tint black innerBoxMoved
      , Draw.tint white outerBoxMoved
      ]
  hover
    = mconcat
      [ Draw.tint white labelMoved
      , Draw.tint gray  innerBoxMoved
      , Draw.tint white outerBoxMoved
      ]
  press
    = mconcat
      [ Draw.tint black labelMoved
      , Draw.tint white outerBoxMoved
      ]

