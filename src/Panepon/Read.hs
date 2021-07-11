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

sample :: [String]
sample =
  [ "......",
    "......",
    "......",
    "...Y..",
    "...C..",
    "...R..",
    "......",
    ".GGR..",
    "CGCRYY"
  ]

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
  let x = length $ head ss
      y = length ss
      ps = toPanels ss
   in Board ps (Grid x y) (Cursor 3 3)