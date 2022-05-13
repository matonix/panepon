{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Panepon.Env where

import Lens.Micro
import Lens.Micro.TH
import qualified Panepon.Game as G
import Prelude hiding (Left, Right)
import Panepon.Board (Board)

type FPS = Int

data KeyEvent
  = Up
  | Down
  | Left
  | Right
  | Confirm
  | Cancel
  deriving (Eq)

data Event
  = Key KeyEvent
  | Tick
  deriving (Eq)

type KeyEvents = [KeyEvent]

data Mode = Game

data Env = Env
  { _mode :: Mode,
    _keys :: KeyEvents,
    _game :: G.Game,
    _fps :: FPS
  }

makeLenses ''Env

next :: Event -> Env -> Env
next Tick env = Env {..}
  where
    _mode = env ^. mode
    _keys = []
    _game = G.next keys' $ env ^. game
      where
        keys' = map keyEventMapper $ env ^. keys
    _fps = env ^. fps
next (Key key) env = env & keys %~ (key :)

keyEventMapper :: KeyEvent -> G.Event
keyEventMapper Up = G.Up
keyEventMapper Down = G.Down
keyEventMapper Left = G.Left
keyEventMapper Right = G.Right
keyEventMapper Confirm = G.Confirm
keyEventMapper Cancel = G.Cancel

-- for debug
initEnv :: Board -> FPS -> IO Env
initEnv board fps = return $ Env {..}
  where
    _mode = Game
    _keys = []
    _game = G.Game board (G.Debug 0)
    _fps = fps

-- マジックナンバーの扱いはTODO
debugFPS :: FPS
debugFPS = 2

ordinaryFPS :: FPS
ordinaryFPS = 60
