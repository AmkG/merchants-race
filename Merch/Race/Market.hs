{- Market model.  -}

{- The market is "efficient".  What this means is,
   each settlement should not have a surplus or a
   deficit.

   Each settlement has to consume items (food, etc)
   to survive, and will produce items.  It will
   also convert items from one to another (e.g.
   towns convert wool to clothes).  Thus, it's
   possible to "stockpile" items, which may be
   negative (if there is demand for that item that
   isn't getting met).  The goal of the market is
   to set this stockpile to 0: no deficits, no
   oversupply.

   Since we don't know the exact market mechanisms,
   and remembering control theeory, we use a PID
   controller.  Each settlement's stockpile for
   each item is the PID's error (since the goal of
   the PID is to set the error to 0).  The output
   of the PID controller is the price of the item
   at that settlement.  -}

module Merch.Race.Market
  ( MarketM(..)
  , market
  , marketInit
  ) where

import Merch.Race.Data

import Control.Monad
import Data.Ratio

{- A MarketM monad is any monad that has operations
   on various market variables.  -}
class Monad m => MarketM m where
  mkGetAllSettlements :: m [Settlement]
  mkGetAllItems :: m [Item]
  mkGetStockpile :: Settlement -> Item -> m Integer
  -- the PID controller needs to retain these variables
  -- across days.
  mkGetPreviousError :: Settlement -> Item -> m Integer
  mkSetPreviousError :: Settlement -> Item -> Integer -> m ()
  mkGetIntegral :: Settlement -> Item -> m Integer
  mkSetIntegral :: Settlement -> Item -> Integer -> m ()
  -- The PID outputs the desired market price.
  mkSetTargetPrice :: Settlement -> Item -> Price -> m ()

{- Settings for the PID.  -}
{- The center price.  The PID outputs a signed value, but
   the price should be a positive value.  So we need to
   adjust this by adding the center price below.  -}
centerPrice :: Price
centerPrice = 500
{- PID tunables.  THe value kp is the proportionality
   constant, ki is the integral constant, kd is the
   derivative constant.  We expect the signal to be
   noisy (due to randomness in production and
   consumption) so we should have a very low derivative
   constant and a somewhat high integral constant.  -}
kp, ki, kd :: Ratio Integer
kp = 0.25 ; ki = 1.0 ; kd = 0.05

{- A PID controller is:
   previousError = 0
   integral = 0
   while(1)
     error = setpoint() - actual()
     integral = integral + error * dt
     derivative = (error - previousError) / dt
     output(kp*error + ki*integral + kd*derivative)
     previousError = error

   marketInit below is the initialization part, while
   market is the loop body.  The market function is
   expected to be called once a day, so its "dt"
   is 1.
-}
marketInit :: MarketM m => m ()
marketInit = do
  settlements <- mkGetAllSettlements
  items <- mkGetAllItems
  forM_ [(s,i)|s <- settlements, i <- items] $ \(settlement, item) -> do
    mkSetPreviousError settlement item 0
    mkSetIntegral settlement item 0
    mkSetTargetPrice settlement item centerPrice

market :: MarketM m => m ()
market = do
  settlements <- mkGetAllSettlements
  items <- mkGetAllItems
  forM_ [(s,i)|s <- settlements, i <- items] $ \(settlement, item) -> do
    stockpile <- mkGetStockpile settlement item
    -- The error is the negation of the stockpile, since
    -- a deficit should *increase* the price.
    let error = negate stockpile

    prevIntegral <- mkGetIntegral settlement item
    let integral = prevIntegral + error
    mkSetIntegral settlement item integral

    previousError <- mkGetPreviousError settlement item
    let derivative = error - previousError

    let outputRatio = kp * (error % 1) + ki * (integral % 1) + kd * (derivative % 1)
        output = numerator outputRatio `div` denominator outputRatio
        targetPrice = fromIntegral output + centerPrice
    mkSetTargetPrice settlement item targetPrice

    mkSetPreviousError settlement item error
