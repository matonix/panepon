module Main where

import Panepon.Board
import Panepon.Read
import Panepon.Render
import qualified Panepon.TUI as TUI
import Prelude hiding (Left, Right)
import Panepon.Game
import Panepon.Env
import Lens.Micro
import Panepon.Rule

main :: IO ()
main = do
  let board = toBoard sample & rule .~ ordinaryRule 
  game <- initGame board
  let env = Env game ordinaryFPS
  TUI.tuiMain env

-- main = do
--   let board = toBoard sample
--   loop 0 events board
--   where
--     loop 14 _ _ = return ()
--     loop t (e : es) board = do
--       putStrLn $ "t = " ++ show t
--       putStrLn $ render board
--       -- print board
--       loop (t + 1) es (next e board)

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

sample2 :: [String]
sample2 =
  [ "......",
    "......",
    "......",
    "......",
    "......",
    "......",
    "......",
    "......",
    "......"
  ]

events :: [Events]
events = [[Down], [Down, Left], [Left], [Swap], [Right], [Right, Down], [Right, Up], [Right], [Right]] ++ repeat []

events2 :: [Events]
events2 = [Lift] : repeat []
