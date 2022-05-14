{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.TUI.Main where

import Brick hiding (Down, Up, render)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Lens.Micro ((&), (.~), (^.))
import qualified Panepon.Env as Env
import qualified Panepon.Game as Game
import Panepon.TUI.Render (Name, drawUI, theMap)
import System.TimeIt (timeItT)
import Prelude hiding (Left, Right)

data Tick = Tick

-- App definition

tuiMain :: Env.Env -> IO ()
tuiMain env = do
  chan <- newBChan 10
  _ <- forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay $ 1000000 `div` env ^. Env.fps
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app env

app :: App Env.Env Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

-- Handling events

handleEvent :: Env.Env -> BrickEvent Name Tick -> EventM Name (Next Env.Env)
handleEvent e (AppEvent Tick) = do
  (d, e') <- liftIO $ timeItT (return $ Env.next Env.Tick e)
  continue $ e' & Env.game . Game.debug . Game.duration .~ d
handleEvent e (VtyEvent (V.EvKey V.KUp [])) = continue $ Env.next (Env.Key Env.Up) e
handleEvent e (VtyEvent (V.EvKey V.KDown [])) = continue $ Env.next (Env.Key Env.Down) e
handleEvent e (VtyEvent (V.EvKey V.KRight [])) = continue $ Env.next (Env.Key Env.Right) e
handleEvent e (VtyEvent (V.EvKey V.KLeft [])) = continue $ Env.next (Env.Key Env.Left) e
handleEvent e (VtyEvent (V.EvKey (V.KChar 'x') [])) = continue $ Env.next (Env.Key Env.Confirm) e
handleEvent e (VtyEvent (V.EvKey (V.KChar 'z') [])) = continue $ Env.next (Env.Key Env.Cancel) e
-- handleEvent e (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initGame >>= continue
handleEvent e (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt e -- 終了を正しく扱うのはTODO
handleEvent e (VtyEvent (V.EvKey V.KEsc [])) = halt e
handleEvent e _ = continue e
