{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.List
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Lens.Micro
import Panepon.Board
import qualified Panepon.Cursor as C
import qualified Panepon.Grid as G
import qualified Panepon.Panel as P
import Panepon.Game
import Panepon.Render (Render, render)
import System.TimeIt (timeItT)
import Text.Printf
import Prelude hiding (Left, Right)
import Panepon.Env

data Tick = Tick

type Name = ()

-- App definition

tuiMain :: Env -> IO ()
tuiMain env = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay $ 1000000 `div` env ^. fps
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app $ env ^. game

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
handleEvent g (AppEvent Tick) = do
  (d, g') <- liftIO $ timeItT (step g)
  continue $ g' & debug . duration .~ d
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

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [C.center $ padRight (Pad 2) (drawStats g) <+> render g]

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 24 $
    vBox
      [ drawDebugInfo (g ^. board) (g ^. debug),
        padTop (Pad 2) $ drawGameOver (g ^. board . dead)
      ]

drawDebugInfo :: Board -> Debug -> Widget Name
drawDebugInfo board debug =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Info") $
      C.hCenter $
        vBox
          [ drawStrShow "combo" $ board ^. combo,
            drawStrShow "chain" $ board ^. chain,
            drawStrShow "lift" $ board ^. grid . G.lift,
            drawStrShow "forceMode" $ board ^. grid . G.forceMode,
            drawStrShow "liftEvent" $ board ^. grid . G.prevEvent,
            drawStr "duration" $ printf "%.4fms" $ debug ^. duration . to (* 1000)
          ]
  where
    drawStr name dat = str (name ++ ": " ++ dat)
    drawStrShow name dat = str (name ++ ": " ++ show dat)

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
    then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
    else emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

instance Render Game (Widget Name) where
  render (Game _events board _debug) = render board

instance Render Board (Widget Name) where
  render board =
    hLimit (w * 2 + 3) $
      vLimit (h + 2) $
        withBorderStyle BS.unicodeBold $
          B.borderWithLabel (str "Panepon") $
            vBox rows
    where
      C.Cursor cx cy = board ^. cursor
      (w, h) = board ^. grid . to G.getBound
      ps = board ^. panels
      rows = reverse [hBox $ cellsInRow j | j <- [0 .. h]]
      cellsInRow j = renderCursor cx cy 0 j : concat [renderPanel i j | i <- [1 .. w]]
      renderPanel i j = [maybe renderEmpty render maybePanel, maybe id colorAttr maybeColor $ renderCursor cx cy i j]
        where
          maybePanel = find ((== (i, j)) . P._pos) ps
          maybeColor = fmap P._color maybePanel

renderEmpty :: Widget Name
renderEmpty = str " "

renderCursor :: Int -> Int -> Int -> Int -> Widget Name
renderCursor x y i j
  | i == x - 1 && j == y = withAttr cursorAttr $ str "["
  | i == x + 1 && j == y = withAttr cursorAttr $ str "]"
  | otherwise = str " "

instance Render P.Panel (Widget Name) where
  render p = colorAttr (P._color p) $ renderDebug p

colorAttr :: P.Color -> Widget Name -> Widget Name
colorAttr P.Red = withAttr redAttr
colorAttr P.Green = withAttr greenAttr
colorAttr P.Cyan = withAttr cyanAttr
colorAttr P.Purple = withAttr purpleAttr
colorAttr P.Yellow = withAttr yellowAttr
colorAttr P.Blue = withAttr blueAttr

redAttr, greenAttr, cyanAttr, purpleAttr, yellowAttr, blueAttr, cursorAttr :: AttrName
redAttr = "redAttr"
greenAttr = "greenAttr"
cyanAttr = "cyanAttr"
purpleAttr = "purpleAttr"
yellowAttr = "yellowAttr"
blueAttr = "blueAttr"
cursorAttr = "cursorAttr"

renderDebug :: P.Panel -> Widget Name
renderDebug (P._state -> P.Init) = str "X"
renderDebug (P._state -> P.Move P.L) = str "←"
renderDebug (P._state -> P.Move P.R) = str "→"
renderDebug (P._state -> P.Float) = str "☁"
renderDebug (P._state -> P.Fall) = str "↓"
renderDebug (P._state -> P.Vanish) = str "☼"
renderDebug (P._state -> P.Empty) = str "E"
renderDebug (P._color -> P.Red) = str "❤"
renderDebug (P._color -> P.Green) = str "■"
renderDebug (P._color -> P.Cyan) = str "▲"
renderDebug (P._color -> P.Purple) = str "◆"
renderDebug (P._color -> P.Yellow) = str "★"
renderDebug (P._color -> P.Blue) = str "▼"

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