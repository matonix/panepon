{-# LANGUAGE ViewPatterns #-}

module Panepon.Grid where

import Control.Arrow
import Data.Ratio

-- 1-indexed grid, i.e., 1..w, 1..h and -d..0
-- lift represents ratio to the height of panel, i.e., lift == 1 -> lift complete
data Grid = Grid
  { width :: Int,
    height :: Int,
    depth :: Int,
    lift :: Rational
  }
  deriving (Show)

data Event
  = Auto Int -- frame / panel
  | Force Int -- frame / panel

getBound :: Grid -> (Int, Int)
getBound = width &&& height

next :: Event -> Grid -> Grid
next (Auto f) grid@(lift -> l) = grid {lift = zeroToOne $ l + 1 % fromIntegral f}
next (Force f) grid@(lift -> l) = grid {lift = zeroToOne $ l + 1 % fromIntegral f}

-- >>> zeroToOne 1.2
-- 1 % 5
-- >>> zeroToOne 0.25
-- 1 % 4
-- >>> zeroToOne 1 == 0
-- True
zeroToOne :: Rational -> Rational
zeroToOne r = r - fromInteger (numerator r `div` denominator r)
