{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.Grid where

import Control.Arrow (Arrow ((&&&)))
import Data.Ratio ((%))
import Lens.Micro ((&), (.~))
import Lens.Micro.TH (makeLenses)

-- 1-indexed grid, i.e., 1..w, 1..h and -d..0
-- lift represents ratio to the height of panel, i.e., lift == 1 -> lift complete
data Grid = Grid
  { _width :: Int,
    _height :: Int,
    _depth :: Int,
    _lift :: Rational,
    _liftComplete :: Bool,
    _forceMode :: Bool,
    _prevEvent :: Event
  }
  deriving (Show)

data Event
  = Auto Int -- frame / panel
  | Force Int -- frame / panel
  | Stop
  deriving (Show, Eq)

makeLenses ''Grid

getBound :: Grid -> (Int, Int)
getBound = _width &&& _height

next :: Event -> Grid -> Grid
next (Auto f) grid@(_lift -> l) = let nextLift = tick l f in grid & lift .~ nextLift & liftComplete .~ (nextLift == 0) & forceMode .~ False & prevEvent .~ Auto f
next (Force f) grid@(_lift -> l) = let nextLift = tick l f in grid & lift .~ nextLift & liftComplete .~ (nextLift == 0) & forceMode .~ True & prevEvent .~ Force f
next Stop grid = grid & liftComplete .~ False & prevEvent .~ Stop

tick :: Rational -> Int -> Rational
tick l f = zeroToOne $ l + 1 % fromIntegral f

-- >>> zeroToOne 1.2
-- 0 % 1
-- >>> zeroToOne 0.25
-- 1 % 4
-- >>> zeroToOne 1 == 0
-- True
zeroToOne :: Rational -> Rational
zeroToOne r = if r >= 1 then 0 else r
