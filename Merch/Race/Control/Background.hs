{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{- Merch.Race.Control.Background - Background task execution.

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

{- Execution of code in a background thread.
   The foreground thread can sample any available
   state that the background thread reports. -}
module Merch.Race.Control.Background
  ( Background --          * -> *
  , sampleBackground --    Background s -> IO (Maybe s)
  , completedBackground -- Background s -> IO (Maybe s)
  , abortBackground --     Background s -> IO ()

  , BackM --               * -> * -> *
  , runBackM --            (s' -> IO s) -> BackM s' a -> s' -> IO (Background s)
  ) where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State
import Control.Monad.Trans

newtype Background s
  = Background
    { bkState :: MVar (BackState s)
    }
data BackState s
  = Running
  | SampleAvailable s
  | Finished s
  | RequestSample
  | Excepted SomeException
  | Aborted

newtype BackM s a
  = BackM
    { bmRun :: s -> (s -> a -> IO (Event s)) -> IO (Event s)
    }
data Event s
  = Put s (IO (Event s))
  | EExcepted SomeException
  | Finish s

instance Monad (BackM s) where
  return a = BackM $ \s k -> k s a
  fail e = BackM $ \_ _ -> fail e
  ma >>= f = BackM $ \s k -> bmRun ma s $ \s a -> bmRun (f a) s k
instance MonadState s (BackM s) where
  get = BackM $ \s k -> k s s
  put s = BackM $ \_ k -> return $ Put s $ k s ()
instance MonadIO (BackM s) where
  liftIO m = BackM $ \s k -> m >>= \a -> k s a

sampleBackground :: Background s -> IO (Maybe s)
sampleBackground bk = do
  let mv = bkState bk
  bs <- takeMVar mv
  case bs of
    Running           -> do
      putMVar mv RequestSample
      return Nothing
    SampleAvailable s -> do
      putMVar mv RequestSample
      return $ Just s
    Finished s        -> do
      putMVar mv bs
      return $ Just s
    RequestSample     -> do
      putMVar mv bs
      return Nothing
    Excepted e        -> do
      putMVar mv bs
      throwIO e
    Aborted           -> do
      putMVar mv bs
      fail "sampleBackground: Background task already aborted."
completedBackground :: Background s -> IO (Maybe s)
completedBackground bk = do
  let mv = bkState bk
  bs <- readMVar mv
  case bs of
    Finished s -> return $ Just s
    Excepted e -> throwIO e
    Aborted    -> do
      fail "completedBackground: Background task already aborted"
    _          -> return Nothing
abortBackground :: Background s -> IO ()
abortBackground bk = do
  let mv = bkState bk
  swapMVar mv Aborted
  return ()

runBackM :: (s' -> IO s) -> BackM s' a -> s' -> IO (Background s)
runBackM freeze ma s' = do
  mv <- newMVar Running
  forkIO $ loop mv $ bmRun ma s' $ \s' _ -> return $ Finish s'
  return $ Background mv
 where
  loop mv ioe = do
    e <- catch ioe $ return . EExcepted
    bs <- takeMVar mv
    case e of
      Put s' ioe   -> do
        case bs of
          Aborted           -> putMVar mv Aborted
          Running           -> putMVar mv bs >> loop mv ioe
          -- We assume that the freeze operation
          -- is not fast, so if the previous sample
          -- is still there, don't freeze the current
          -- sample, leave the foreground with stale
          -- data.
          SampleAvailable _ -> putMVar mv bs >> loop mv ioe
          RequestSample     -> do
            s <- freeze s'
            putMVar mv $ SampleAvailable s
            loop mv ioe
          -- Finished and Excepted shouldn't have occured.
          _                 -> putMVar mv bs
      EExcepted ex -> do
        case bs of
          -- If we got aborted before the
          -- exception triggered, ignore
          Aborted -> putMVar mv Aborted
          _       -> putMVar mv $ Excepted ex
      Finish s'    -> do
        case bs of
          -- If we got aborted before the
          -- finish, ignore.
          Aborted -> putMVar mv Aborted
          _       -> do
            s <- freeze s'
            putMVar mv $ Finished s
