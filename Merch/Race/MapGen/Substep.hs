{- Merch.Race.MapGen.Substep - Substep implementation for map generation code.

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
module Merch.Race.MapGen.Substep
  ( SubMG
  , substep
  ) where

import Merch.Race.MapGen.Monad

newtype SubMG m a
  = SubMG
    { run :: (Rational -> Rational -> m a)
    }

instance Monad m => Monad (SubMG m) where
  return = lift . return
  fail = lift . fail
  ma >>= f = SubMG $ \mult add ->
    run ma mult add >>= flip uncurry (mult, add) . run . f

lift :: m a -> SubMG m a
lift ma = SubMG $ \_ _ -> ma

instance MapGenM m => MapGenM (SubMG m) where
  mgMapBounds = lift mgMapBounds
  mgGetTerrain = lift . mgGetTerrain
  mgPutTerrain h t = lift $ mgPutTerrain h t
  mgGetRoad = lift . mgGetRoad
  mgPutRoad h r = lift $ mgPutRoad h r
  mgStep = lift . mgStep
  mgProgress p = SubMG $ \mult add -> do
    mgProgress $ p * mult + add
  mgRandom = lift mgRandom

substep :: MapGenM m => Rational -> Rational -> SubMG m a -> m a
substep add mult ma = run ma mult add

