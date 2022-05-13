{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Panepon.Game where

import Lens.Micro
import Lens.Micro.TH
import qualified Panepon.Board as B
import Prelude hiding (Left, Right)

data Event
  = Up
  | Down
  | Left
  | Right
  | Confirm
  | Cancel
  deriving (Eq)

type Events = [Event]

newtype Debug = Debug
  { _duration :: Double
  }

makeLenses ''Debug

data Game = Game
  { _board :: B.Board,
    _debug :: Debug
  }

makeLenses ''Game

next :: Events -> Game -> Game
next events game = Game {..}
  where
    _board = B.next events' $ game ^. board
      where
        events' = map eventMapper events
    _debug = _debug

eventMapper :: Event -> B.Event
eventMapper Up = B.Up
eventMapper Down = B.Down
eventMapper Left = B.Left
eventMapper Right = B.Right
eventMapper Confirm = B.Swap
eventMapper Cancel = B.Lift
