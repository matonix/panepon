{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.Render where

import Data.Foldable
import Lens.Micro
import Panepon.Board
import Panepon.Cursor
import Panepon.Grid
import Panepon.Panel

class Render a b where
  render :: a -> b

instance Render Panel String where
  render (_state -> Init) = "X"
  render (_state -> Move L) = "←"
  render (_state -> Move R) = "→"
  render (_state -> Float) = "F"
  render (_state -> Fall) = "↓"
  render (_state -> Vanish) = "V"
  render (_state -> Empty) = "E"
  render (_color -> Red) = "❤"
  render (_color -> Green) = "□"
  render (_color -> Cyan) = "▲"
  render (_color -> Purple) = "◇"
  render (_color -> Yellow) = "★"
  render (_color -> Blue) = "▽"

instance Render Board String where
  render board =
    unlines $
      reverse -- 上から下へ
        [ concat
            [ if
                  | i == cx && j == cy -> "["
                  | i == cx + 2 && j == cy -> "]"
                  | otherwise -> " "
                ++ maybe " " render (find ((== (i, j)) . _pos) ps)
              | i <- [1 .. w + 1]
            ]
          | j <- [1 .. h]
        ]
    where
      Cursor cx cy = board ^. cursor
      (w, h) = board ^. grid . to getBound
      ps = board ^. panels