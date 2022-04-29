-- |
-- Module      : FRP.Yampa.Vty
-- Description : Vty backend for Yampa
-- Copyright   : Kevin Mullins 2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
module FRP.Yampa.Vty (
  runVty,
  Command (..),
) where

import Data.Functor (($>))
import Data.IORef (atomicModifyIORef', newIORef)
import FRP.Yampa (DTime, Event (..), SF, event, reactimate)
import Graphics.Vty (Vty (..))
import Graphics.Vty qualified as Vty (Event (..), Picture, displayBounds, setWindowTitle)
import System.Clock (Clock (..), TimeSpec (..), diffTimeSpec, getTime)

-- | Vty commands
data Command
  = -- | Refresh the terminal. See `refresh`
    Refresh
  | -- | Set the window title. See `Vty.setWindowTitle`
    SetTitle String
  | -- | Terminate the program. See `shutdown`
    Terminate

-- | Run a signal function with the given `Vty`.
runVty ::
  -- | The `Vty` instance to use
  Vty ->
  -- | The signal function to run
  SF (Event Vty.Event) (Event Vty.Picture, Event Command) ->
  IO ()
runVty vty@Vty {nextEventNonblocking, outputIface, refresh, shutdown, update} sf = do
  start <- getTime Monotonic
  clock <- newIORef start

  let initialSize = do
        (x, y) <- Vty.displayBounds outputIface
        pure (Event (Vty.EvResize x y))
      pollClock = do
        t' <- getTime Monotonic
        atomicModifyIORef' clock \t -> (t', toDTime (t `diffTimeSpec` t'))
      pollInput _ = do
        e <- nextEventNonblocking
        time' <- pollClock
        pure (maybe (time', Just NoEvent) ((time',) . Just . Event) e)
      actuate _ (picture, command) = do
        event (pure ()) update picture
        case command of
          NoEvent -> pure False
          Event Refresh -> refresh $> False
          Event (SetTitle title) -> Vty.setWindowTitle vty title $> False
          Event Terminate -> shutdown $> True

  reactimate initialSize pollInput actuate sf

toDTime :: TimeSpec -> DTime
toDTime TimeSpec {sec, nsec} = fromIntegral sec + fromIntegral nsec / 1e9
