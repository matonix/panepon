{-# LANGUAGE TemplateHaskell #-}
module Panepon.Game where

import Lens.Micro
import Lens.Micro.TH
import Panepon.Board

newtype Debug = Debug
  { _duration :: Double
  }

makeLenses ''Debug

data Game = Game
  { _events :: Events,
    _board :: Board,
    _debug :: Debug
  }

makeLenses ''Game

step :: Game -> IO Game
step game = return $ game & board %~ next es & events .~ []
  where
    es = game ^. events

turn :: Event -> Game -> Game
turn event game = game & events %~ (event :)

initGame :: Board -> IO Game
initGame board = return $ Game [] board (Debug 0)
