module Panepon.Cursor where

import Prelude hiding (Left, Right)

data Cursor = Cursor Int Int
  deriving (Eq, Show)

data Event
  = Up
  | Down
  | Left
  | Right
  | None

next :: Event -> Cursor -> Cursor
next Up (Cursor x y) = Cursor x (y + 1)
next Down (Cursor x y) = Cursor x (y - 1)
next Left (Cursor x y) = Cursor (x - 1) y
next Right (Cursor x y) = Cursor (x + 1) y
next None cursor = cursor
