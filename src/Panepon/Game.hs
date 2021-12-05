{-# LANGUAGE TemplateHaskell #-}

module Panepon.Game where

import Lens.Micro
import Lens.Micro.TH
import Panepon.Board (Board, Event, Events)
import qualified Panepon.Board as B

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
step game = return $ game & board %~ B.next es & events .~ []
  where
    es = game ^. events

next :: Event -> Game -> Game
next event game = game & events %~ (event :)

initGame :: Board -> IO Game
initGame board = return $ Game [] board (Debug 0)
