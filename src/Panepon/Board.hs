{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.Board where

import Data.Foldable
import qualified Data.List as L
import qualified Data.List.NonEmpty as N
import Data.Maybe
import Lens.Micro
import Lens.Micro.TH
import qualified Panepon.Cursor as C
import qualified Panepon.Grid as G
import qualified Panepon.Panel as P
import Panepon.Rule
import Prelude hiding (Left, Right)

type Name = ()

type Panels = [P.Panel]

class ColorGenerator g where
  getNext :: g -> (P.Color, g)
  mkGen :: [P.Color] -> g

-- deterministic
newtype DetGen = DetGen (N.NonEmpty P.Color)

instance ColorGenerator DetGen where
  getNext (DetGen (c N.:| cs)) = (c, DetGen (N.fromList cs))
  mkGen availableColors = DetGen $ N.cycle $ N.fromList $ concat $ L.permutations availableColors

instance Show DetGen where
  show = const "DetGen"

data Board = Board
  { _rule :: Rule,
    _panels :: Panels,
    _grid :: G.Grid,
    _cursor :: C.Cursor,
    _gen :: DetGen,
    _combo :: Int,
    _chain :: Int,
    _score :: Int,
    _dead :: Bool
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
next events (Board rule panels grid cursor gen combo chain score False) =
  let grid' = nextGrid rule events panels grid
      cursor' = nextCursor events grid' cursor
      (panels', gen', combo', chain', chain_up, dead') = nextPanels rule events grid' cursor' panels gen combo chain
      score' = nextScore rule score combo' chain' chain_up
   in Board rule panels' grid' cursor' gen' combo' chain' score' dead'
next events board@(Board _ _ _ _ _ _ _ _ True) = board

nextScore :: Rule -> Int -> Int -> Int -> Bool -> Int
nextScore rule score combo chain chain_up = score + chainScore + comboScore
  where
    chainScore = if chain_up then lookupScoreTable (rule ^. chainTable) chain else 0
    comboScore = if combo > 0 then lookupScoreTable (rule ^. comboTable) combo else 0

nextGrid :: Rule -> Events -> Panels -> G.Grid -> G.Grid
nextGrid rule events panels grid
  | not $ all (\(P._state -> s) -> s == P.Init || s == P.Idle) panels = G.next G.Stop grid
  --  | has (each . P.state . to (== P.Init)) panels = G.next G.Stop grid
  | Lift `elem` events || (G._forceMode grid && not (G._liftComplete grid)) = G.next (G.Force $ rule ^. forceLiftFinish) grid
  | otherwise = G.next (G.Auto $ rule ^. liftFinish) grid

nextCursor :: Events -> G.Grid -> C.Cursor -> C.Cursor
nextCursor events grid cursor = foldr (C.next . toCursorEvent cursor) cursor events'
  where
    (w, h) = G.getBound grid
    events' = if G._liftComplete grid then events ++ [Up] else events
    toCursorEvent :: C.Cursor -> Event -> C.Event
    toCursorEvent (C.Cursor _x y) Up | y < h = C.Up
    toCursorEvent (C.Cursor _x y) Down | y > 1 = C.Down
    toCursorEvent (C.Cursor x _y) Left | x > 1 = C.Left
    toCursorEvent (C.Cursor x _y) Right | x + 1 < w = C.Right
    toCursorEvent _ _ = C.None

genPanel :: ColorGenerator g => g -> Panels -> P.Pos -> (P.Panel, g)
genPanel gen panels pos@(i, j) = (P.Panel c P.Init 0 pos False Nothing Nothing, gen')
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
        sameOrNothing (Just a) (Just b) | a ^. P.color == b ^. P.color = a ^? P.color
        sameOrNothing _ _ = Nothing
        muc = sameOrNothing mu1 mu2
        mlc = sameOrNothing ml1 ml2
        mrc = sameOrNothing mr1 mr2

genPanels :: ColorGenerator g => g -> Panels -> [P.Pos] -> (Panels, g)
genPanels gen panels poss = go poss gen panels
  where
    go [] gen ps = (ps, gen)
    go (pos : poss) gen ps = let (p, gen') = genPanel gen ps pos in go poss gen' (p : ps)

-- >>> genGround (mkGen::DetGen) [] (G.Grid 2 9 0 0 False False G.Stop)
-- ([Panel {_color = Green, _state = Init, _count = 0, _pos = (2,0), _chainable = False},Panel {_color = Red, _state = Init, _count = 0, _pos = (1,0), _chainable = False}],DetGen)
genGround :: ColorGenerator g => g -> Panels -> G.Grid -> (Panels, g)
genGround gen panels grid = genPanels gen panels [(i, - G._depth grid) | i <- [1 .. G._width grid]]

nextPanels :: ColorGenerator g => Rule -> Events -> G.Grid -> C.Cursor -> Panels -> g -> Int -> Int -> (Panels, g, Int, Int, Bool, Bool)
nextPanels rule events grid cursor panels gen combo chain = (ss, gen', combo', chain', chain_up, dead)
  where
    -- tick event
    te = tickEvent panels
    -- lift event
    (le, gen') = liftEvent te gen grid
    -- count finish
    cf = countFinish rule le
    -- bottom condition
    bc = bottomCondition cf
    -- empty collect
    ec = emptyCollect bc
    -- combo start
    cs = comboStart ec
    -- combo
    (combo', cc) = comboCount cs
    -- chain
    (chain', chain_up) = chainCount cc chain
    -- swap start
    ss = swapStart cc events cursor
    -- check dead
    dead = checkDead grid ss

tickEvent :: Panels -> Panels
tickEvent = map (P.next P.Tick)

liftEvent :: ColorGenerator g => Panels -> g -> G.Grid -> (Panels, g)
liftEvent panels gen grid =
  if G._liftComplete grid
    then
      let liftApplied = map (P.next P.Lift) panels
          available = flip map liftApplied $ \p@(P._pos -> snd -> j) -> if j > 0 then P.next P.Available p else p
       in genGround gen available grid
    else (panels, gen)

-- #TODO: next ?????????????????????????????????????????????????????????
countFinish :: Rule -> Panels -> Panels
countFinish rule panels =
  flip map panels $
    P.next (P.CountFinish (P.Move P.L) $ rule ^. moveFinish)
      . P.next (P.CountFinish (P.Move P.R) $ rule ^. moveFinish)
      . P.next (P.CountFinish P.Float $ rule ^. floatFinish)
      . P.next (P.CountFinish P.Fall $ rule ^. fallFinish)
-- Vanish state: Blink -(vanishBlink)-> Wait -(vanishWait + vanishPanel * ix)-> Vanished -(vanishPanel * (total - ix))-> Empty
      . P.next (P.CountFinish (P.Vanish P.Blink) $ rule ^. vanishBlink)
      . (\panel -> P.next (P.CountFinish (P.Vanish P.Wait) $ rule ^. vanishWait + rule ^. vanishPanel * fromJust (panel ^. P.vanishIx)) panel)
      . (\panel -> P.next (P.CountFinish (P.Vanish P.Vanished) $ rule ^. vanishPanel * (fromJust (panel ^. P.vanishTotal) - fromJust (panel ^. P.vanishIx))) panel)
-- #TODO fromJust ????????????????????????????????????????????????

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

comboCount :: Panels -> (Int, Panels)
comboCount panels = (len, nextPanels)
  where
    (comboPanels, nonConboPanels) = L.partition (\p -> P._state p == P.Vanish P.Blink && P._count p == 0) panels
    len = length comboPanels
    sorted = L.sortBy (\(P._pos -> (x1, y1)) (P._pos -> (x2, y2)) -> compare y2 y1 <> compare x1 x2) comboPanels
    comboPanels' = zipWith3 (\ix total p -> p & P.vanishIx ?~ ix & P.vanishTotal ?~ total) [0..] (repeat len) sorted
    nextPanels = nonConboPanels ++ comboPanels'

chainCount :: Panels -> Int -> (Int, Bool)
chainCount panels chain
  | chainInc = (chain + 1, True)
  | chainContinue = (chain, False)
  | anyVanishing = (1, False)
  | otherwise = (0, False)
  where
    chainInc = any (\p -> P._state p == P.Vanish P.Blink && P._count p == 0 && P._chainable p) panels
    chainContinue = not . all (\p -> P._state p == P.Init || (P._state p == P.Idle && P._count p > 0)) $ filter P._chainable panels
    anyVanishing = any (\p -> case P._state p of
      P.Vanish _ -> True
      _ -> False ) panels

swapStart :: Panels -> Events -> C.Cursor -> Panels
swapStart panels events (C.Cursor x y) =
  if Swap `elem` events
    then flip map panels $ \p@(P._pos -> (i, j)) ->
      if
          | i == x && j == y -> P.next (P.Swap P.R) p
          | i == x + 1 && j == y -> P.next (P.Swap P.L) p
          | otherwise -> p
    else panels

checkDead :: G.Grid -> Panels -> Bool
checkDead grid panels =
  grid ^. G.prevEvent /= G.Stop
    && grid ^. G.liftComplete
    && any (\p -> p ^. P.pos . _2 == grid ^. G.height) panels