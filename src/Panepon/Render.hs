{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.Render where

import Data.Foldable
import Panepon.Board
import Panepon.Cursor
import Panepon.Grid
import Panepon.Panel

class Render a b where
  render :: a -> b

instance Render Panel String where
  render (state -> Init) = "X"
  render (state -> Move L) = "←"
  render (state -> Move R) = "→"
  render (state -> Float) = "F"
  render (state -> Fall) = "↓"
  render (state -> Vanish) = "V"
  render (state -> Empty) = "E"
  render (color -> Red) = "❤"
  render (color -> Green) = "□"
  render (color -> Cyan) = "▲"
  render (color -> Purple) = "◇"
  render (color -> Yellow) = "★"
  render (color -> Blue) = "▽"

instance Render Board String where
  render (Board panels (Grid x y) (Cursor x' y')) =
    unlines $
      reverse -- 上から下へ
        [ concat
            [ if
                  | i == x' && j == y' -> "["
                  | i == x' + 2 && j == y' -> "]"
                  | otherwise -> " "
                ++ maybe " " render (find ((== (i, j)) . pos) panels)
              | i <- [1 .. x + 1]
            ]
          | j <- [1 .. y]
        ]