module Panepon.Read where

import Data.Maybe
import Panepon.Board
import Panepon.Cursor
import Panepon.Grid
import Panepon.Panel

parse :: Char -> Maybe Color
parse 'R' = Just Red
parse 'G' = Just Green
parse 'C' = Just Cyan
parse 'P' = Just Purple
parse 'Y' = Just Yellow
parse 'B' = Just Blue
parse _ = Nothing

toPanels :: [String] -> [Panel]
toPanels ss =
  concat
    [ catMaybes
        [ case parse c of
            Just col -> Just $ Panel col Idle 0 (x, y)
            Nothing -> Nothing
          | (c, x) <- zip s [1 ..]
        ]
      | (s, y) <- zip (reverse ss) [1 ..] -- 下から読む
    ]

toBoard :: [String] -> Board
toBoard ss =
  let w = length $ head ss
      h = length ss
      ps = toPanels ss
      dummyGrounds = [Panel c Init 0 (i, 0) | i <- [1 .. w], let c = if even i then Purple else Yellow]
   in Board (ps ++ dummyGrounds) (Grid w h 0 0) (Cursor 3 3)