module Panepon.Board where

import Panepon.Grid
import Panepon.Panel

data Board = Board
  { panels :: [Panel],
    grid :: Grid
  }