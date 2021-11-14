{-# LANGUAGE TemplateHaskell #-}

module Panepon.Rule where

import Lens.Micro.TH ( makeLenses )
import Panepon.Panel (Color (..))

data Rule = Rule
  { _moveFinish :: Int,
    _floatFinish :: Int,
    _fallFinish :: Int,
    _vanishFinish :: Int,
    _liftFinish :: Int,
    _forceLiftFinish :: Int,
    _availableColors :: [Color]
  }
  deriving (Show)

makeLenses ''Rule

debugRule :: Rule
debugRule =
  Rule
    { _moveFinish = 1,
      _floatFinish = 1,
      _fallFinish = 1,
      _vanishFinish = 1,
      _liftFinish = 2,
      _forceLiftFinish = 3,
      _availableColors = [Red, Green, Cyan, Purple, Yellow, Blue]
    }

ordinaryRule :: Rule
ordinaryRule =
  Rule
    { _moveFinish = 10,
      _floatFinish = 60,
      _fallFinish = 1,
      _vanishFinish = 300,
      _liftFinish = 300,
      _forceLiftFinish = 60,
      _availableColors = [Red, Green, Cyan, Purple, Yellow, Blue]
    }