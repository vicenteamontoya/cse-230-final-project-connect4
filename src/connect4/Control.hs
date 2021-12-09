module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Network.WebSockets as WS
import Data.Text

import Model
import Model.Board
import View

-------------------------------------------------------------------------------

control :: GlobalState -> BrickEvent n Tick -> EventM n (Next GlobalState)
control s (T.VtyEvent (V.EvKey V.KEsc _)) = halt s -- Esc to Quit Game anytime
control s@(GS (Play p) conn _) ev = case ev of
  AppEvent (Tick "LEFT")             -> continue s { state = (initEndMenu (-1)) } -- Other player leaves
  AppEvent (Tick msg)                -> nextGameS p s $ playServer p (read msg) -- Receive column number
  T.VtyEvent (V.EvKey V.KEnter _)    -> case play p of
    Retry -> continue s
    _     -> do
      liftIO $ WS.sendTextData conn (pack $ show $ psCol p) -- Send column number
      nextGameS p s $ play p
  T.VtyEvent (V.EvKey V.KLeft _)     -> continue s { state = (Play (move left p)) }
  T.VtyEvent (V.EvKey V.KRight _)    -> continue s { state = (Play (move right p)) }
  _                                  -> continue s
control s@(GS (MainMenu n) conn _) ev = case ev of
  T.VtyEvent (V.EvKey V.KEnter _)    -> case n of
    4 -> halt s
    3 -> do
      liftIO $ WS.sendTextData conn (pack "PLAY")
      continue s { state = Loading }
    _ -> continue s { state = (mainMenuSelect n) }
  T.VtyEvent (V.EvKey (V.KChar c) _) -> continue s { state = MainMenu $ keyToInt mainMenuOptionCount n c }
  T.VtyEvent (V.EvKey V.KDown _)     -> continue s { state = MainMenu ((n + 1) `mod` mainMenuOptionCount) }
  T.VtyEvent (V.EvKey V.KUp _)       -> continue s { state = MainMenu ((n - 1) `mod` mainMenuOptionCount) }
  _                                  -> continue s
control s@(GS (EndMenu (EMS r n)) _ _) ev = case ev of
  T.VtyEvent (V.EvKey V.KEnter _)    -> if n == 3 then halt s else continue s { state = (endMenuSelect n) }
  T.VtyEvent (V.EvKey (V.KChar c) _) -> continue s { state = EndMenu $ EMS r (keyToInt endMenuOptionCount n c) }
  T.VtyEvent (V.EvKey V.KDown _)     -> continue s { state = EndMenu $ EMS r ((n + 1) `mod` endMenuOptionCount) }
  T.VtyEvent (V.EvKey V.KUp _)       -> continue s { state = EndMenu $ EMS r ((n - 1) `mod` endMenuOptionCount) }
  _                                  -> continue s
control s@(GS Loading _ _) ev = case ev of
  AppEvent (Tick msg)                -> continue s { state = initMultiplayerGame msg }
  _                                  -> continue s
control s@(GS Instructions _ _) ev = case ev of
  T.VtyEvent (V.EvKey V.KLeft _)     -> continue s { state = initMainMenu }
  _                                  -> continue s
control s@(GS (Settings n) _ _) ev = case ev of
  T.VtyEvent (V.EvKey V.KEnter _)    -> continue (settingsSelect s n)
  T.VtyEvent (V.EvKey (V.KChar c) _) -> continue s { state = (Settings $ keyToInt settingsOptionCount n c) }
  T.VtyEvent (V.EvKey V.KDown _)     -> continue s { state = (Settings ((n `mod` settingsOptionCount) + 1)) }
  T.VtyEvent (V.EvKey V.KUp _)       -> continue s { state = (Settings (((n - 2) `mod` settingsOptionCount) + 1)) }
  _                                  -> continue s

-------------------------------------------------------------------------------
move :: (Int -> Int) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psCol = f (psCol s) }

-------------------------------------------------------------------------------
play :: PlayState -> Result Board
-------------------------------------------------------------------------------
play s = case psTurn s of
  R -> case psR s of
    Local  -> put (psBoard s) R (psCol s)
    Server -> Retry
  B -> case psB s of
    Local  -> put (psBoard s) B (psCol s)
    Server -> Retry

-------------------------------------------------------------------------------
playServer :: PlayState -> Int -> Result Board
-------------------------------------------------------------------------------
playServer s n = case psTurn s of
  R -> case psR s of
    Server -> put (psBoard s) R n
    Local  -> Retry
  B -> case psB s of
    Server -> put (psBoard s) B n
    Local  -> Retry

-------------------------------------------------------------------------------
nextGameS :: PlayState -> GlobalState -> Result Board -> EventM n (Next GlobalState)
-------------------------------------------------------------------------------
nextGameS p s r = case r of
  Retry  -> continue s { state = (Play p) }
  Cont b -> continue s { state = (Play $ p {psBoard = b, psTurn = (flipRB $ psTurn p)}) }
  Draw   -> continue s { state = (initEndMenu 0) } -- Draw is 0
  Win R  -> continue s { state = (initEndMenu 1) } -- Red win is 1
  Win B  -> continue s { state = (initEndMenu 2) } -- Blue win is 2, other player left so you win is -1

mainMenuSelect :: Int -> State
mainMenuSelect n = case n of
  0 -> initLocalGame
  2 -> Instructions
  3 -> initSettings
  _ -> initMainMenu -- Impossible State (Loading at 1 and Quit at 4 handled above)

endMenuSelect :: Int -> State
endMenuSelect n = case n of
  0 -> initMainMenu
  1 -> initSettings
  _ -> initMainMenu -- Impossible State (Quit at 2 handled above)

settingsSelect :: GlobalState -> Int -> GlobalState
settingsSelect gs@(GS _ _ sl) n = case n of
  0 -> gs { setting = sl { colorScheme = (((colorScheme sl) `mod` colorSchemeCount) + 1) } } -- update colors
  1 -> gs { setting = sl { diskChar    = (((diskChar sl) `mod` diskCharCount) + 1) } } -- update chars
  2 -> gs { setting = sl { diskShape   = (((diskShape sl) `mod` diskShapeCount) + 1) } } -- update shape
  3 -> gs { state = initMainMenu } -- Back to main menu
  _ -> gs -- Impossible State

-- Args: max value, default value, char
keyToInt :: Int -> Int -> Char -> Int
keyToInt m n c = if (k > 0) && (k <= m) then k else n
  where
    k = fromEnum c - fromEnum '0'