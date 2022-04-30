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

import Control.Concurrent (threadDelay)
import Data.Functor (($>))
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Int (Int64)
import FRP.Yampa (DTime, Event (..), SF, event, reactimate)
import Graphics.Vty (Vty (..))
import Graphics.Vty qualified as Vty (Event (..), Picture, displayBounds, setWindowTitle)
import System.Clock (Clock (..), TimeSpec (..), getTime)

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
  -- | Maximum framerate in Hz
  Word ->
  -- | The signal function to run
  SF (Event Vty.Event) (Event Vty.Picture, Event Command) ->
  IO ()
runVty vty@Vty {nextEventNonblocking, outputIface, refresh, shutdown, update} fps sf = do
  let frameLength = round ((1e6 :: Double) / fromIntegral fps)

  start <- getTime Monotonic
  clock <- newIORef start

  let initialSize = do
        (x, y) <- Vty.displayBounds outputIface
        pure (Event (Vty.EvResize x y))
      pollClock = do
        t' <- getTime Monotonic
        delta <- atomicModifyIORef' clock \t -> (t', (t' - t))
        let remaining = frameLength - toμs delta
        if remaining <= 0
          then pure (toDTime delta)
          else do
            threadDelay (fromIntegral remaining)
            t'' <- getTime Monotonic
            delta' <- atomicModifyIORef' clock \t -> (t'', (t'' - t))
            (pure . toDTime) (delta + delta')
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

toμs :: TimeSpec -> Int64
toμs TimeSpec {sec, nsec} = sec * 1e6 + (nsec `div` 1e3)
