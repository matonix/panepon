{-# LANGUAGE ViewPatterns #-}

module Panepon.Panel where

data Color = Red | Green | Cyan | Purple | Yellow | Blue
  deriving (Eq)

data State = Init | Idle | Move Direction | Float | Fall | Vanish | Empty

data Direction = L | R

type Count = Int

type Pos = (Int, Int)

data Panel = Panel
  { color :: Color,
    state :: State,
    count :: Count,
    pos :: Pos
  }

-- イベントの発行は発行者の責任（チェックはPanel側では行わない）
data Event
  = Tick
  | Lift
  | CountFinish Count
  | Bottom State Count
  | Available
  | Combo
  | Swap Direction

isGround :: State -> Bool
isGround Idle = True
isGround (Move _) = True
isGround Vanish = True
isGround _ = False

isMovable :: State -> Bool
isMovable Idle = True
isMovable (Move _) = True
isMovable Fall = True
isMovable _ = False

next :: Event -> Panel -> Panel
next Tick panel@(count -> c) = panel {count = c + 1}
next Lift panel@(pos -> (x, y)) = panel {pos = (x, y + 1)}
next (CountFinish c) panel@(state -> Move d) | count panel == c = panel {state = Idle, count = 0}
next (CountFinish c) panel@(state -> Float) | count panel == c = panel {state = Fall, count = 0}
next (CountFinish c) panel@(state -> Vanish) | count panel == c = panel {state = Empty, count = 0}
next (Bottom Empty _) panel@(state -> Idle) = panel {state = Float, count = 0}
next (Bottom Fall _) panel@(state -> Idle) = panel {state = Fall, count = 0}
next (Bottom Float c) panel@(state -> Fall) = panel {state = Float, count = c}
next (Bottom b _) panel@(state -> Fall) | isGround b = panel {state = Idle, count = 0}
next Available panel@(state -> Init) = panel {state = Idle}
next Combo panel@(state -> Idle) = panel {state = Vanish, count = 0}
next (Swap d) panel@(state -> Idle) = panel {state = Move d, count = 0}
next (Swap d) panel@(state -> Idle) = panel {state = Move d, count = 0}
next (Swap d) panel@(state -> s) | isMovable s = panel {state = Move d, count = 0}
next _ panel = panel
