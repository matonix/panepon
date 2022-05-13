{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Panepon.TUI where
-- TODO: Render(ViewModel) と アプリケーションの制御（Event）を分類すべき

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
import Data.Ratio (denominator, numerator)
import qualified Graphics.Vty as V
import Lens.Micro
import Panepon.Board (Board, Event (..))
import qualified Panepon.Board as B
import qualified Panepon.Cursor as C
import qualified Panepon.Env as Env
import qualified Panepon.Game as Game
import qualified Panepon.Grid as G
import qualified Panepon.Panel as P
import Panepon.Render (Render, render)
import System.TimeIt (timeItT)
import Text.Printf
import Prelude hiding (Left, Right)

data Tick = Tick

type Name = ()

-- App definition

tuiMain :: Env.Env -> IO ()
tuiMain env = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay $ 1000000 `div` env ^. Env.fps
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app env

app :: App Env.Env Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

-- Handling events

handleEvent :: Env.Env -> BrickEvent Name Tick -> EventM Name (Next Env.Env)
handleEvent e (AppEvent Tick) = do
  (d, e') <- liftIO $ timeItT (return $ Env.next Env.Tick e)
  continue $ e' & Env.game . Game.debug . Game.duration .~ d
handleEvent e (VtyEvent (V.EvKey V.KUp [])) = continue $ Env.next (Env.Key Env.Up) e
handleEvent e (VtyEvent (V.EvKey V.KDown [])) = continue $ Env.next (Env.Key Env.Down) e
handleEvent e (VtyEvent (V.EvKey V.KRight [])) = continue $ Env.next (Env.Key Env.Right) e
handleEvent e (VtyEvent (V.EvKey V.KLeft [])) = continue $ Env.next (Env.Key Env.Left) e
handleEvent e (VtyEvent (V.EvKey (V.KChar 'x') [])) = continue $ Env.next (Env.Key Env.Confirm) e
handleEvent e (VtyEvent (V.EvKey (V.KChar 'z') [])) = continue $ Env.next (Env.Key Env.Cancel) e
-- handleEvent e (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initGame >>= continue
handleEvent e (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt e -- 終了を正しく扱うのはTODO
handleEvent e (VtyEvent (V.EvKey V.KEsc [])) = halt e
handleEvent e _ = continue e

-- Drawing

drawUI :: Env.Env -> [Widget Name]
drawUI env = [render env]

instance Render Env.Env (Widget Name) where
  render env = render $ env ^. Env.game

instance Render Game.Game (Widget Name) where
  render game@(Game.Game board _debug) = 
    C.center $ padRight (Pad 2) (drawStats game) <+> render board

drawStats :: Game.Game -> Widget Name
drawStats g =
  hLimit 25 $
    vBox
      [ drawDebugInfo (g ^. Game.board) (g ^. Game.debug),
        padTop (Pad 1) $ drawGameOver (g ^. Game.board . B.dead)
      ]

drawDebugInfo :: Board -> Game.Debug -> Widget Name
drawDebugInfo board debug =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Info") $
      C.hCenter $
        vBox
          [ drawStrShow "combo" $ board ^. B.combo,
            drawStrShow "chain" $ board ^. B.chain,
            drawStrShow "score" $ board ^. B.score,
            drawStr "lift" $ printf "%2d%%" $ board ^. B.grid . G.lift . to (\r -> 100 * numerator r `div` denominator r),
            drawStrShow "forceMode" $ board ^. B.grid . G.forceMode,
            drawStrShow "liftEvent" $ board ^. B.grid . G.prevEvent,
            drawStr "duration" $ printf "%.4fms" $ debug ^. Game.duration . to (* 1000)
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

instance Render Board (Widget Name) where
  render board =
    hLimit (w * 2 + 3) $
      vLimit (h + 2) $
        withBorderStyle BS.unicodeBold $
          B.borderWithLabel (str "Panepon") $
            vBox rows
    where
      C.Cursor cx cy = board ^. B.cursor
      (w, h) = board ^. B.grid . to G.getBound
      ps = board ^. B.panels
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