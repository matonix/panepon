module Main where

import Panepon.Read
import Panepon.Render

main :: IO ()
main = putStrLn $ render $ toBoard sample
