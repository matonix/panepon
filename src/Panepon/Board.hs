{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE FlexibleContexts #-}
module Panepon.Board where

import Data.Foldable
import Data.List
import Data.Maybe
import qualified Panepon.Cursor as C
import qualified Panepon.Grid as G
import qualified Panepon.Panel as P
import Prelude hiding (Left, Right)
import Lens.Micro.TH
import Lens.Micro

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

type Panels = [P.Panel]

class ColorGenerator g where
  getNext :: g -> (P.Color, g)
  mkGen :: g

-- deterministic
-- #TODO 記述順が影響するのを回避する（mkLensesの前にすべて宣言済みとなる必要がある）
newtype DetGen = DetGen [P.Color]

instance ColorGenerator DetGen where
  getNext (DetGen (c : cs)) = (c, DetGen cs)
  mkGen = DetGen $ cycle $ concat $ permutations availableColors

instance Show DetGen where
  show = const "DetGen"

data Board = Board
  { _panels :: Panels,
    _grid :: G.Grid,
    _cursor :: C.Cursor,
    _gen :: DetGen,
    _combo :: Int,
    _chain :: Int
  }
  deriving (Show)

makeLenses ''Board

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
  | not $ all (\(P._state -> s) -> s == P.Init || s == P.Idle) panels = G.next G.Stop grid
  --  | has (each . P.state . to (== P.Init)) panels = G.next G.Stop grid
  | Lift `elem` events || (G._forceMode grid && not (G._liftComplete grid)) = G.next (G.Force forceLiftFinish) grid
  | otherwise = G.next (G.Auto liftFinish) grid

nextCursor :: Events -> G.Grid -> C.Cursor -> C.Cursor
nextCursor events grid cursor = foldr (C.next . toCursorEvent cursor) cursor events'
  where
    (w, h) = G.getBound grid
    events' = if G._lift grid == 0 then events ++ [Up] else events
    toCursorEvent :: C.Cursor -> Event -> C.Event
    toCursorEvent (C.Cursor x y) Up | y < h = C.Up
    toCursorEvent (C.Cursor x y) Down | y > 1 = C.Down
    toCursorEvent (C.Cursor x y) Left | x > 1 = C.Left
    toCursorEvent (C.Cursor x y) Right | x + 1 < w = C.Right
    toCursorEvent _ _ = C.None

genPanel :: ColorGenerator g => g -> Panels -> P.Pos -> (P.Panel, g)
genPanel gen panels pos@(i, j) = (P.Panel c P.Init 0 pos False, gen')
  where
    (c, gen') = go gen ngs
      where
        go gen ngs = let (c, gen') = getNext gen in if c `elem` ngs then go gen' ngs else (c, gen')
    ngs = catMaybes [muc, mlc, mrc]
      where
        mu1 = find ((== (i, j + 1)) . P._pos) panels
        mu2 = find ((== (i, j + 2)) . P._pos) panels
        ml1 = find ((== (i - 1, j)) . P._pos) panels
        ml2 = find ((== (i - 2, j)) . P._pos) panels
        mr1 = find ((== (i + 1, j)) . P._pos) panels
        mr2 = find ((== (i + 2, j)) . P._pos) panels
        sameOrNothing (Just a) (Just b) | P._color a == P._color b = Just $ P._color a
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
genGround gen panels grid = genPanels gen panels [(i, - G._depth grid) | i <- [1 .. G._width grid]]

nextPanels :: ColorGenerator g => Events -> G.Grid -> C.Cursor -> Panels -> g -> Int -> Int -> (Panels, g, Int, Int)
nextPanels events grid cursor panels gen combo chain = (ss, gen', combo', chain')
  where
    -- tick event
    te = tickEvent panels
    -- lift event
    (le, gen') = liftEvent te gen grid
    -- count finish
    cf = countFinish le
    -- bottom condition
    bc = bottomCondition cf
    -- empty collect
    ec = emptyCollect bc
    -- combo start
    cs = comboStart ec
    -- combo
    combo' = comboCount cs
    -- chain
    chain' = chainCount cs chain
    -- swap start
    ss = swapStart cs events cursor

tickEvent :: Panels -> Panels
tickEvent = map (P.next P.Tick)

liftEvent :: ColorGenerator g => Panels -> g -> G.Grid -> (Panels, g)
liftEvent panels gen grid = genGround gen lifted grid
  where
    lifted =
      if G._liftComplete grid
        then
          let liftApplied = map (P.next P.Lift) panels
              available = flip map liftApplied $ \p@(P._pos -> snd -> j) -> if j > 0 then P.next P.Available p else p
           in available
        else panels

countFinish :: Panels -> Panels
countFinish panels =
  flip map panels $
    P.next (P.CountFinish (P.Move P.L) moveFinish)
      . P.next (P.CountFinish (P.Move P.R) moveFinish)
      . P.next (P.CountFinish P.Float floatFinish)
      . P.next (P.CountFinish P.Fall fallFinish)
      . P.next (P.CountFinish P.Vanish vanishFinish)

bottomCondition :: Panels -> Panels
bottomCondition panels = loop panels []
  where
    loop curr prev
      | curr == prev = curr
      | otherwise = loop next curr
      where
        bottoms = flip map curr $ \(P._pos -> (i, j)) -> find ((== (i, j - 1)) . P._pos) curr
        next = flip map (zip curr bottoms) $ \(p, mp) ->
          let s = maybe P.Empty P._state mp
              c = maybe 0 P._count mp
              ch = maybe False (\p' -> P._chainable p' || P._state p' == P.Empty) mp
           in P.next (P.Bottom s c ch) p

emptyCollect :: Panels -> Panels
emptyCollect = filter ((/= P.Empty) . P._state)

comboStart :: Panels -> Panels
comboStart panels = flip map panels $ \p@(P._pos -> pos) -> if elem pos comboableH || elem pos comboableV then P.next P.Combo p else p
  where
    comboableH = comboable (pred, id) (succ, id) panels
    comboableV = comboable (id, succ) (id, pred) panels
    comboable (ia, ja) (ib, jb) panels = concat $
      flip map panels $ \p ->
        let (i, j) = P._pos p
            ma = find ((== (ia i, ja j)) . P._pos) panels
            mb = find ((== (ib i, jb j)) . P._pos) panels
         in case (ma, mb) of
              (Just a, Just b)
                | all ((P.Idle ==) . P._state) [p, a, b]
                    && all ((P._color p ==) . P._color) [a, b] ->
                  map P._pos [p, a, b]
              _ -> []

comboCount :: Panels -> Int
comboCount = length . filter (\p -> P._state p == P.Vanish && P._count p == 0)

chainCount :: Panels -> Int -> Int
chainCount panels chain
  | chainInc = chain + 1
  | chainContinue = chain
  | anyVanishing = 1
  | otherwise = 0
  where
    chainInc = any (\p -> P._state p == P.Vanish && P._count p == 0 && P._chainable p) panels
    chainContinue = not . all (\p -> P._state p == P.Init || (P._state p == P.Idle && P._count p > 0)) $ filter P._chainable panels
    anyVanishing = any (\p -> P._state p == P.Vanish) panels

swapStart :: Panels -> Events -> C.Cursor -> Panels
swapStart panels events (C.Cursor x y) =
  if Swap `elem` events
    then flip map panels $ \p@(P._pos -> (i, j)) ->
      if
          | i == x && j == y -> P.next (P.Swap P.R) p
          | i == x + 1 && j == y -> P.next (P.Swap P.L) p
          | otherwise -> p
    else panels