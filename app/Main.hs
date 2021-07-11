module Main where

import Panepon.Board
import Panepon.Read
import Panepon.Render
import Prelude hiding (Left, Right)

main :: IO ()
main = do
  let board = toBoard sample
  let events = [[Down], [Down, Left], [Left], [Swap], [Left], [Left]] ++ repeat []
  loop 0 events board
  where
    loop 13 _ _ = return ()
    loop t (e : es) board = do
      putStrLn $ "t = " ++ show t
      putStrLn $ render board
      -- print board
      loop (t + 1) es (next e board)
