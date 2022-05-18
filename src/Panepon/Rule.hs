{-# LANGUAGE TemplateHaskell #-}

module Panepon.Rule where

import Data.IMap (IMap, Run (..))
import qualified Data.IMap as IMap
import Data.Maybe (fromMaybe)
import Lens.Micro.TH (makeLenses)
import Panepon.Panel (Color (..))

type ScoreTable = (Int, IMap Int)

data Rule = Rule
  { _moveFinish :: Int,
    _floatFinish :: Int,
    _fallFinish :: Int,
    _vanishBlink :: Int,
    _vanishWait :: Int,
    _vanishPanel :: Int,
    _liftFinish :: Int,
    _forceLiftFinish :: Int,
    _availableColors :: [Color],
    _chainTable :: ScoreTable,
    _comboTable :: ScoreTable
  }
  deriving (Show)

makeLenses ''Rule

debugRule :: Rule
debugRule =
  Rule
    { _moveFinish = 1,
      _floatFinish = 1,
      _fallFinish = 1,
      _vanishBlink = 1,
      _vanishWait = 1,
      _vanishPanel = 1,
      _liftFinish = 2,
      _forceLiftFinish = 3,
      _availableColors = [Red, Green, Cyan, Purple, Yellow, Blue],
      _chainTable = chainTableGC,
      _comboTable = comboTableGC
    }

ordinaryRule :: Rule
ordinaryRule =
  Rule
    { _moveFinish = 10,
      _floatFinish = 30,
      _fallFinish = 1,
      _vanishBlink = 30,
      _vanishWait = 30,
      _vanishPanel = 20,
      _liftFinish = 300,
      _forceLiftFinish = 60,
      _availableColors = [Red, Green, Cyan, Purple, Yellow, Blue],
      _chainTable = chainTableGC,
      _comboTable = comboTableGC
    }

lookupScoreTable :: ScoreTable -> Int -> Int
lookupScoreTable (def, map) k = fromMaybe def (IMap.lookup k map)

chainTableGC :: ScoreTable
chainTableGC =
  ( 1800,
    IMap.fromList $
      zip
        [1 ..]
        $ map
          (Run 1)
          [ 0,
            50,
            80,
            150,
            300,
            400,
            500,
            700,
            900,
            1100,
            1300,
            1500,
            1800
          ]
  )

comboTableGC :: ScoreTable
comboTableGC =
  ( 46000,
    IMap.fromList $
      zip
        [3 ..]
        $ map
          (Run 1)
          [ 30,
            60,
            150,
            190,
            230,
            270,
            310,
            400,
            450,
            500,
            550,
            700,
            760,
            850,
            970,
            1120,
            1300,
            1510,
            1750,
            2020,
            2320,
            2650,
            3010,
            3400,
            3820,
            4270,
            4750,
            5260,
            15000,
            15570,
            16170,
            16800,
            17460,
            18150,
            18870,
            19620,
            20400,
            21200,
            22000,
            22800,
            23600,
            24400,
            25200,
            26000,
            26800,
            27600,
            28400,
            29200,
            30000,
            30800,
            31600,
            32400,
            33200,
            34000,
            34800,
            35600,
            36400,
            37200,
            38000,
            38800,
            39600,
            40400,
            41200,
            42000,
            42800,
            43600,
            44400,
            45200,
            46000
          ]
  )