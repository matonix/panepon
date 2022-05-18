{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.Panel where

import Lens.Micro.TH ( makeLenses )
import Lens.Micro ( Lens', (&), (%~), (.~), _1, _2 )

data Color = Red | Green | Cyan | Purple | Yellow | Blue
  deriving (Eq, Show)

data VState = Blink | Wait | Vanished
  deriving (Eq, Show)

data State = Init | Idle | Move Direction | Float | Fall | Vanish VState | Empty
  deriving (Eq, Show)

data Direction = L | R
  deriving (Eq, Show)

type Count = Int

type Pos = (Int, Int)

data Panel = Panel
  { _color :: Color,
    _state :: State,
    _count :: Count,
    _pos :: Pos,
    _chainable :: Bool,
    _vanishIx :: Maybe Int,
    _vanishTotal :: Maybe Int
  }
  deriving (Eq, Show)

makeLenses ''Panel

-- イベントの発行は発行者の責任（チェックはPanel側では行わない）
data Event
  = Tick
  | Lift
  | CountFinish State Count
  | Bottom State Count Bool
  | Available
  | Combo
  | Swap Direction

isGround :: State -> Bool
isGround Init = True
isGround Idle = True
isGround Move{} = True
isGround Vanish{} = True
isGround _ = False

isMovable :: State -> Bool
isMovable Idle = True
isMovable (Move _) = True
isMovable Fall = True
isMovable _ = False

next :: Event -> Panel -> Panel
next Tick panel = panel & count %~ succ
next Lift panel = panel & posY %~ succ
next (CountFinish (Move d) c) panel@(_state -> Move d') | _count panel == c && d == d' = panel & state .~ Idle & countReset
next (CountFinish Float c) panel@(_state -> Float) | _count panel == c = panel & state .~ Fall & countReset
next (CountFinish Fall c) panel@(_state -> Fall) | _count panel == c = panel & state .~ Fall & countReset & posY %~ pred
next (CountFinish (Vanish Blink) c) panel@(_state -> Vanish Blink) | _count panel == c = panel & state .~ Vanish Wait & countReset
next (CountFinish (Vanish Wait) c) panel@(_state -> Vanish Wait) | _count panel == c = panel & state .~ Vanish Vanished & countReset
next (CountFinish (Vanish Vanished) c) panel@(_state -> Vanish Vanished) | _count panel == c = panel & state .~ Empty & countReset
next (Bottom Empty _ ch) panel@(_state -> Idle) = panel & state .~ Float & countReset & chainable .~ ch
next (Bottom Fall _ ch) panel@(_state -> Idle) = panel & state .~ Fall & countReset & chainable .~ ch
next (Bottom Float _ _) panel@(_state -> Fall) = panel & state .~ Float & countReset
next (Bottom b _ _) panel@(_state -> Fall) | isGround b = panel & state .~ Idle & countReset
next (Bottom b _ _) panel@(_state -> Idle) | isGround b && _count panel > 0 = panel & state .~ Idle & chainable .~ False
next Available panel@(_state -> Init) = panel & state .~ Idle
next Combo panel@(_state -> Idle) = panel & state .~ Vanish Blink & countReset
next (Swap L) panel@(_state -> s) | isMovable s = panel & state .~ Move L & countReset & posX %~ pred
next (Swap R) panel@(_state -> s) | isMovable s = panel & state .~ Move R & countReset & posX %~ succ
next _ panel = panel

countReset :: Panel -> Panel
countReset = count .~ 0

posX :: Lens' Panel Int
posX = pos . _1

posY :: Lens' Panel Int
posY = pos . _2