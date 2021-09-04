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
  render (Board panels (getBound -> (w, h)) (Cursor x y) _ combo chain) =
    unlines $
      ("conbo: " ++ show combo ++ ", chain: " ++ show chain) :
      reverse -- 上から下へ
        [ concat
            [ if
                  | i == x && j == y -> "["
                  | i == x + 2 && j == y -> "]"
                  | otherwise -> " "
                ++ maybe " " render (find ((== (i, j)) . _pos) panels)
              | i <- [1 .. w + 1]
            ]
          | j <- [1 .. h]
        ]