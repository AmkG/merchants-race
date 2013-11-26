
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

