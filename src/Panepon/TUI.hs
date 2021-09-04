{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.TUI where

import Brick hiding (Down, Left, Right, Up, render)
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Panepon.Board
import Panepon.Cursor (Cursor (Cursor))
import Panepon.Grid (getBound)
import Panepon.Panel (Color (..), Direction (..), Panel, State (..), _color, _pos, _state)
import Panepon.Render (Render, render)
import Prelude hiding (Left, Right)

data Game = Game
  { events :: Events,
    board :: Board
  }

data Tick = Tick

type Name = ()

-- App definition

debugMain :: Board -> IO ()
debugMain b = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 500000 -- #TODO make config for setting FPS
  g <- initGame b
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ turn Up g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ turn Down g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ turn Right g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ turn Left g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'x') [])) = continue $ turn Swap g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'z') [])) = continue $ turn Lift g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initGame >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g

-- #TODO implement "Env" layer steps

step :: Game -> Game
step g@(Game events board) = g {board = next events board, events = []}

turn :: Event -> Game -> Game
turn event g@(Game events _) = g {events = event : events}

initGame :: Board -> IO Game
initGame board = return $ Game [] board

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [C.center $ padRight (Pad 2) (drawStats g) <+> render g]

drawStats :: Game -> Widget Name
drawStats g =
  vBox
    []

-- drawScore :: Int -> Widget Name
-- drawScore n =
--   withBorderStyle BS.unicodeBold $
--     B.borderWithLabel (str "Score") $
--       C.hCenter $
--         padAll 1 $
--           str $ show n

-- drawGameOver :: Bool -> Widget Name
-- drawGameOver dead =
--   if dead
--     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
--     else emptyWidget

-- gameOverAttr :: AttrName
-- gameOverAttr = "gameOver"

instance Render Game (Widget Name) where
  render (Game _events board) = render board

instance Render Board (Widget Name) where
  render (Board panels (getBound -> (w, h)) (Cursor x y) _ _ _) =
    hLimit (w * 2 + 3) $
      vLimit (h + 2) $
        withBorderStyle BS.unicodeBold $
          B.borderWithLabel (str "Panepon") $
            vBox rows
    where
      rows = reverse [hBox $ cellsInRow j | j <- [0 .. h]]
      cellsInRow j = renderCursor x y 0 j : concat [renderPanel i j | i <- [1 .. w]]
      renderPanel i j = [maybe renderEmpty render maybePanel, maybe id colorAttr maybeColor $ renderCursor x y i j]
        where
          maybePanel = find ((== (i, j)) . _pos) panels
          maybeColor = fmap _color maybePanel

renderEmpty :: Widget Name
renderEmpty = str " "

renderCursor :: Int -> Int -> Int -> Int -> Widget Name
renderCursor x y i j
  | i == x - 1 && j == y = withAttr cursorAttr $ str "["
  | i == x + 1 && j == y = withAttr cursorAttr $ str "]"
  | otherwise = str " "

instance Render Panel (Widget Name) where
  render p = colorAttr (_color p) $ renderDebug p

colorAttr :: Color -> Widget Name -> Widget Name
colorAttr Red = withAttr redAttr
colorAttr Green = withAttr greenAttr
colorAttr Cyan = withAttr cyanAttr
colorAttr Purple = withAttr purpleAttr
colorAttr Yellow = withAttr yellowAttr
colorAttr Blue = withAttr blueAttr

redAttr, greenAttr, cyanAttr, purpleAttr, yellowAttr, blueAttr, cursorAttr :: AttrName
redAttr = "redAttr"
greenAttr = "greenAttr"
cyanAttr = "cyanAttr"
purpleAttr = "purpleAttr"
yellowAttr = "yellowAttr"
blueAttr = "blueAttr"
cursorAttr = "cursorAttr"

renderDebug :: Panel -> Widget Name
renderDebug (_state -> Init) = str "X"
renderDebug (_state -> Move L) = str "←"
renderDebug (_state -> Move R) = str "→"
renderDebug (_state -> Float) = str "☁"
renderDebug (_state -> Fall) = str "↓"
renderDebug (_state -> Vanish) = str "☼"
renderDebug (_state -> Empty) = str "E"
renderDebug (_color -> Red) = str "❤"
renderDebug (_color -> Green) = str "■"
renderDebug (_color -> Cyan) = str "▲"
renderDebug (_color -> Purple) = str "◆"
renderDebug (_color -> Yellow) = str "★"
renderDebug (_color -> Blue) = str "▼"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (redAttr, V.black `on` V.red),
      (greenAttr, V.black `on` V.green),
      (cyanAttr, V.black `on` V.cyan),
      (purpleAttr, V.black `on` V.magenta),
      (yellowAttr, V.black `on` V.yellow),
      (blueAttr, V.black `on` V.blue),
      (cursorAttr, V.currentAttr `V.withStyle` V.bold `V.withForeColor` V.white)
    ]