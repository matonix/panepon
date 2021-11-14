{-# LANGUAGE TemplateHaskell #-}

module Panepon.Env where

import Lens.Micro
import Lens.Micro.TH
import Panepon.Game

type FPS = Int

data Env = Env
  { _game :: Game,
    _fps :: FPS
  }

makeLenses ''Env

debugFPS :: FPS
debugFPS = 2

ordinaryFPS :: FPS
ordinaryFPS = 60
