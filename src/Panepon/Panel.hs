{-# LANGUAGE ViewPatterns #-}

module Panepon.Panel where

data Color = Red | Green | Cyan | Purple | Yellow | Blue
  deriving (Eq, Show)

data State = Init | Idle | Move Direction | Float | Fall | Vanish | Empty
  deriving (Eq, Show)

data Direction = L | R
  deriving (Eq, Show)

type Count = Int

type Pos = (Int, Int)

data Panel = Panel
  { color :: Color,
    state :: State,
    count :: Count,
    pos :: Pos,
    chainable :: Bool
  }
  deriving (Eq, Show)

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
next (CountFinish (Move d) c) panel@(state -> Move d') | count panel == c && d == d' = panel {state = Idle, count = 0}
next (CountFinish Float c) panel@(state -> Float) | count panel == c = panel {state = Fall, count = 0}
next (CountFinish Fall c) panel@(state -> Fall) | count panel == c = let (x, y) = pos panel in panel {state = Fall, count = 0, pos = (x, y - 1)}
next (CountFinish Vanish c) panel@(state -> Vanish) | count panel == c = panel {state = Empty, count = 0}
next (Bottom Empty _ chainable) panel@(state -> Idle) = panel {state = Float, count = 0, chainable = chainable}
next (Bottom Fall _ chainable) panel@(state -> Idle) = panel {state = Fall, count = 0, chainable = chainable}
next (Bottom Float c _) panel@(state -> Fall) = panel {state = Float, count = c}
next (Bottom b _ _) panel@(state -> Fall) | isGround b = panel {state = Idle, count = 0}
next (Bottom b _ _) panel@(state -> Idle) | isGround b && count panel > 0 = panel {state = Idle, chainable = False}
next Available panel@(state -> Init) = panel {state = Idle}
next Combo panel@(state -> Idle) = panel {state = Vanish, count = 0}
next (Swap L) panel@(state -> s) | isMovable s = let (x, y) = pos panel in panel {state = Move L, count = 0, pos = (x - 1, y)}
next (Swap R) panel@(state -> s) | isMovable s = let (x, y) = pos panel in panel {state = Move R, count = 0, pos = (x + 1, y)}
next _ panel = panel
