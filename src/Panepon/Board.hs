{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.Board where

import Data.Foldable
import Data.List
import Data.Maybe
import qualified Panepon.Cursor as C
import qualified Panepon.Grid as G
import qualified Panepon.Panel as P
import Prelude hiding (Left, Right)

type Panels = [P.Panel]

data Board = Board
  { panels :: Panels,
    grid :: G.Grid,
    cursor :: C.Cursor,
    gen :: DetGen,
    combo :: Int,
    chain :: Int
  }
  deriving (Show)

-- rules

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
forceLiftFinish = 3

availableColors :: [P.Color]
availableColors = [P.Red, P.Green, P.Cyan, P.Purple, P.Yellow, P.Blue]

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
next events (Board panels grid cursor gen combo chain) =
  let grid' = nextGrid events panels grid
      cursor' = nextCursor events grid' cursor
      (panels', gen', combo', chain') = nextPanels events grid' cursor' panels gen combo chain
   in Board panels' grid' cursor' gen' combo' chain'

nextGrid :: Events -> Panels -> G.Grid -> G.Grid
nextGrid events panels grid
  | not $ all (\(P.state -> s) -> s == P.Init || s == P.Idle || s == P.Empty) panels = G.next G.Stop grid
  | Lift `elem` events || (G.forceMode grid && not (G.liftComplete grid)) = G.next (G.Force forceLiftFinish) grid
  | otherwise = G.next (G.Auto liftFinish) grid

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

class ColorGenerator g where
  getNext :: g -> (P.Color, g)
  mkGen :: g

-- deterministic
newtype DetGen = DetGen [P.Color]

instance ColorGenerator DetGen where
  getNext (DetGen (c : cs)) = (c, DetGen cs)
  mkGen = DetGen $ cycle $ concat $ permutations availableColors

instance Show DetGen where
  show = const "DetGen"

genPanel :: ColorGenerator g => g -> Panels -> P.Pos -> (P.Panel, g)
genPanel gen panels pos@(i, j) = (P.Panel c P.Init 0 pos, gen')
  where
    (c, gen') = go gen ngs
      where
        go gen ngs = let (c, gen') = getNext gen in if c `elem` ngs then go gen' ngs else (c, gen')
    ngs = catMaybes [muc, mlc, mrc]
      where
        mu1 = find ((== (i, j + 1)) . P.pos) panels
        mu2 = find ((== (i, j + 2)) . P.pos) panels
        ml1 = find ((== (i - 1, j)) . P.pos) panels
        ml2 = find ((== (i - 2, j)) . P.pos) panels
        mr1 = find ((== (i + 1, j)) . P.pos) panels
        mr2 = find ((== (i + 2, j)) . P.pos) panels
        sameOrNothing (Just a) (Just b) | P.color a == P.color b = Just $ P.color a
        sameOrNothing _ _ = Nothing
        muc = sameOrNothing mu1 mu2
        mlc = sameOrNothing ml1 ml2
        mrc = sameOrNothing mr1 mr2

genPanels :: ColorGenerator g => g -> Panels -> [P.Pos] -> (Panels, g)
genPanels gen panels poss = go poss gen panels
  where
    go [] gen ps = (ps, gen)
    go (pos : poss) gen ps = let (p, gen') = genPanel gen ps pos in go poss gen' (p : ps)

-- >>> genGround (mkGen::DetGen) [] (G.Grid 2 9 0 0 False)
-- ([Panel {color = Green, state = Init, count = 0, pos = (2,0)},Panel {color = Red, state = Init, count = 0, pos = (1,0)}],DetGen)
genGround :: ColorGenerator g => g -> Panels -> G.Grid -> (Panels, g)
genGround gen panels grid = genPanels gen panels [(i, - G.depth grid) | i <- [1 .. G.width grid]]

nextPanels :: ColorGenerator g => Events -> G.Grid -> C.Cursor -> Panels -> g -> Int -> Int -> (Panels, g, Int, Int)
nextPanels events grid (C.Cursor x' y') panels gen combo chain =
  let -- tick event
      ticked = map (P.next P.Tick) panels
      -- lift
      lifted =
        if G.liftComplete grid
          then
            let liftApplied = map (P.next P.Lift) ticked
                available = flip map liftApplied $ \p ->
                  let (_, j) = P.pos p in if j > 0 then P.next P.Available p else p
             in available
          else ticked
      (groundAdded, gen') = genGround gen lifted grid
      -- count finish
      cf =
        flip map groundAdded $
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
      -- TODO combo
      comboableH = concat $
        flip map bc $ \p ->
          let (i, j) = P.pos p
              ml = find ((== (i - 1, j)) . P.pos) bc
              mr = find ((== (i + 1, j)) . P.pos) bc
           in case (ml, mr) of
                (Just l, Just r)
                  | all ((P.Idle ==) . P.state) [p, l, r]
                      && all ((P.color p ==) . P.color) [l, r] ->
                    map P.pos [p, l, r]
                _ -> []
      comboableV = concat $
        flip map bc $ \p ->
          let (i, j) = P.pos p
              mu = find ((== (i, j + 1)) . P.pos) bc
              md = find ((== (i, j - 1)) . P.pos) bc
           in case (mu, md) of
                (Just u, Just d)
                  | all ((P.Idle ==) . P.state) [p, u, d]
                      && all ((P.color p ==) . P.color) [u, d] ->
                    map P.pos [p, u, d]
                _ -> []
      comboFired = flip map bc $ \p ->
        let pos = P.pos p
         in if elem pos comboableH || elem pos comboableV then P.next P.Combo p else p
      combo' = length $ filter (\p -> P.state p == P.Vanish && P.count p == 0) comboFired
      -- TODO chain
      chain' = chain
      -- swap
      swapped =
        if Swap `elem` events
          then flip map comboFired $ \p ->
            let (i, j) = P.pos p
             in if
                    | i == x' && j == y' -> P.next (P.Swap P.R) p
                    | i == x' + 1 && j == y' -> P.next (P.Swap P.L) p
                    | otherwise -> p
          else comboFired
   in (swapped, gen', combo', chain')
