{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.Board where

import Data.Foldable
import Data.Maybe
import qualified Panepon.Cursor as C
import qualified Panepon.Grid as G
import qualified Panepon.Panel as P
import Prelude hiding (Left, Right)

type Panels = [P.Panel]

data Board = Board
  { panels :: Panels,
    grid :: G.Grid,
    cursor :: C.Cursor
  }
  deriving (Show)

moveFinish :: Int
moveFinish = 1

floatFinish :: Int
floatFinish = 1

fallFinish :: Int
fallFinish = 1

-- TODO パネルの数に応じた変化
vanishFinish :: Int
vanishFinish = 1

liftFinish :: Int
liftFinish = 2

forceLiftFinish :: Int
forceLiftFinish = 1

data Event
  = Up
  | Down
  | Left
  | Right
  | Swap
  | Lift
  deriving (Eq)

type Events = [Event]

next :: Events -> Board -> Board
next events (Board panels grid cursor) =
  let grid' = nextGrid events grid
      cursor' = nextCursor events grid' cursor
      panels' = nextPanels events grid' cursor' panels
   in Board panels' grid' cursor'

nextGrid :: Events -> G.Grid -> G.Grid
nextGrid events grid =
  if Lift `elem` events
    then G.next (G.Force forceLiftFinish) grid
    else G.next (G.Auto liftFinish) grid

nextCursor :: Events -> G.Grid -> C.Cursor -> C.Cursor
nextCursor events grid cursor = foldr (C.next . toCursorEvent cursor) cursor events'
  where
    (w, h) = G.getBound grid
    events' = if G.lift grid == 0 then events ++ [Up] else events
    toCursorEvent :: C.Cursor -> Event -> C.Event
    toCursorEvent (C.Cursor x y) Up | y < h = C.Up
    toCursorEvent (C.Cursor x y) Down | y > 1 = C.Down
    toCursorEvent (C.Cursor x y) Left | x > 1 = C.Left
    toCursorEvent (C.Cursor x y) Right | x + 1 < w = C.Right
    toCursorEvent _ _ = C.None

nextPanels :: Events -> G.Grid -> C.Cursor -> Panels -> Panels
nextPanels events grid (C.Cursor x' y') panels =
  let -- tick event
      ticked = map (P.next P.Tick) panels
      -- TODO lift
      dummyGrounds =
        [ P.Panel c P.Init 0 (i, - G.depth grid)
          | i <- [1 .. G.width grid],
            let c = if even i then P.Purple else P.Yellow
        ]
      lifted =
        if G.lift grid == 0
          then
            let liftApplied = map (P.next P.Lift) ticked
                available = flip map liftApplied $ \p ->
                  let (_, j) = P.pos p in if j > 0 then P.next P.Available p else p
             in available ++ dummyGrounds
          else ticked
      -- count finish
      cf =
        flip map lifted $
          P.next (P.CountFinish (P.Move P.L) moveFinish)
            . P.next (P.CountFinish (P.Move P.R) moveFinish)
            . P.next (P.CountFinish P.Float floatFinish)
            . P.next (P.CountFinish P.Fall fallFinish)
            . P.next (P.CountFinish P.Vanish vanishFinish)
      -- empty collect
      ec = filter ((/= P.Empty) . P.state) cf
      -- bottom condition
      loop curr prev
        | curr == prev = curr
        | otherwise = loop next curr
        where
          bottoms = flip map curr $ \p ->
            let (i, j) = P.pos p
             in find ((== (i, j - 1)) . P.pos) curr
          next = flip map (zip curr bottoms) $ \(p, mp) ->
            let s = maybe P.Empty P.state mp
                c = maybe 0 P.count mp
             in P.next (P.Bottom s c) p
      bc = loop ec []
      -- TODO availables
      avail = bc
      -- TODO combo
      comboableH = concat $
        flip map avail $ \p ->
          let (i, j) = P.pos p
              ml = find ((== (i - 1, j)) . P.pos) avail
              mr = find ((== (i + 1, j)) . P.pos) avail
           in case (ml, mr) of
                (Just l, Just r)
                  | all ((P.Idle ==) . P.state) [p, l, r]
                      && all ((P.color p ==) . P.color) [l, r] ->
                    map P.pos [p, l, r]
                _ -> []
      comboableV = concat $
        flip map avail $ \p ->
          let (i, j) = P.pos p
              mu = find ((== (i, j + 1)) . P.pos) avail
              md = find ((== (i, j - 1)) . P.pos) avail
           in case (mu, md) of
                (Just u, Just d)
                  | all ((P.Idle ==) . P.state) [p, u, d]
                      && all ((P.color p ==) . P.color) [u, d] ->
                    map P.pos [p, u, d]
                _ -> []
      combo = flip map avail $ \p ->
        let pos = P.pos p
         in if elem pos comboableH || elem pos comboableV then P.next P.Combo p else p
      -- TODO swap
      swapped =
        if Swap `elem` events
          then flip map combo $ \p ->
            let (i, j) = P.pos p
             in if
                    | i == x' && j == y' -> P.next (P.Swap P.R) p
                    | i == x' + 1 && j == y' -> P.next (P.Swap P.L) p
                    | otherwise -> p
          else combo
   in swapped
