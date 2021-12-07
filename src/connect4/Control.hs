module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player

-------------------------------------------------------------------------------

control :: GlobalState -> BrickEvent n Tick -> EventM n (Next GlobalState)
control s (T.VtyEvent (V.EvKey V.KEsc _)) = halt s -- Esc to Quit Game anytime
control s@(GS (Play p) conn sl) ev = case ev of 
  -- AppEvent Tick                   -> continue s
  T.VtyEvent (V.EvKey V.KEnter _) -> nextGameS p s =<< liftIO (play p)
  T.VtyEvent (V.EvKey V.KLeft _)  -> continue s { state = (Play (move left p)) }
  T.VtyEvent (V.EvKey V.KRight _) -> continue s { state = (Play (move right p)) }
  _                               -> continue s -- halt s
control s@(GS (MainMenu n) conn sl) ev = case ev of 
  T.VtyEvent (V.EvKey V.KEnter _)    -> if n == 5 then halt s else continue s { state = (mainMenuSelect n) }
  T.VtyEvent (V.EvKey (V.KChar c) _) -> continue s { state = (MainMenu $ keyToInt mainMenuOptionCount n c) }
  T.VtyEvent (V.EvKey V.KDown _)     -> continue s { state = (MainMenu ((n `mod` mainMenuOptionCount) + 1)) }
  T.VtyEvent (V.EvKey V.KUp _)       -> continue s { state = (MainMenu (((n - 2) `mod` mainMenuOptionCount) + 1)) }
  _                                  -> continue s -- halt s
control s@(GS (EndMenu (EMS r n)) conn sl) ev = case ev of 
  T.VtyEvent (V.EvKey V.KEnter _)    -> if n == 3 then halt s else continue s { state = (endMenuSelect n) }
  T.VtyEvent (V.EvKey (V.KChar c) _) -> continue s { state = (EndMenu $ EMS r (keyToInt endMenuOptionCount n c)) }
  T.VtyEvent (V.EvKey V.KDown _)     -> continue s { state = (EndMenu $ EMS r ((n `mod` endMenuOptionCount) + 1)) }
  T.VtyEvent (V.EvKey V.KUp _)       -> continue s { state = (EndMenu $ EMS r (((n - 2) `mod` endMenuOptionCount) + 1)) }
  _                                  -> continue s -- halt s
control s@(GS Loading conn sl) ev = case ev of 
  _                                  -> continue s -- halt s
control s@(GS Instructions conn sl) ev = case ev of 
  T.VtyEvent (V.EvKey V.KLeft _)  -> continue s { state = initMainMenu }
  _                               -> continue s -- halt s  
control s@(GS (Settings n) conn sl) ev = case ev of
  T.VtyEvent (V.EvKey V.KEnter _)    -> continue s { state = (settingsSelect n) }
  T.VtyEvent (V.EvKey (V.KChar c) _) -> continue s { state = (Settings $ keyToInt settingsOptionCount n c) }
  T.VtyEvent (V.EvKey V.KDown _)     -> continue s { state = (Settings ((n `mod` settingsOptionCount) + 1)) }
  T.VtyEvent (V.EvKey V.KUp _)       -> continue s { state = (Settings (((n - 2) `mod` settingsOptionCount) + 1)) }
  _                                  -> continue s -- halt s

-------------------------------------------------------------------------------
move :: (Int -> Int) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psCol = f (psCol s) }

-------------------------------------------------------------------------------
play :: PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
play s
  | psTurn s == rb = put (psBoard s) rb <$> getCol rb s 
  | otherwise      = return Retry
      where rb = psTurn s

getCol :: RB -> PlayState -> IO Int
getCol rb s = getStrategy rb s (psCol s) (psBoard s) rb

getStrategy :: RB -> PlayState -> Strategy 
getStrategy R s = plStrat (psR s)
getStrategy B s = plStrat (psB s)

-------------------------------------------------------------------------------
nextGameS :: PlayState -> GlobalState -> Result Board -> EventM n (Next GlobalState)
-------------------------------------------------------------------------------
nextGameS p s r = case r of
  Retry  -> continue s { state = (Play p) }
  Cont b -> continue s { state = (Play $ p {psBoard = b, psTurn = (flipRB $ psTurn p)}) }
  _      -> continue s { state = (initEndMenu 0) } -- implement result to int

mainMenuSelect :: Int -> State
mainMenuSelect n = case n of
  1 -> initGame
  2 -> initGame
  3 -> Instructions
  4 -> initSettings
  _ -> initMainMenu -- Impossible State (Quit at 5 handled above)

endMenuSelect :: Int -> State
endMenuSelect n = case n of
  1 -> initMainMenu
  2 -> initSettings
  _ -> initMainMenu -- Impossible State (Quit at 3 handled above)

settingsSelect :: Int -> State
settingsSelect n = case n of
  1 -> initMainMenu -- Back to main menu
  2 -> initMainMenu -- TODO update colors
  3 -> initMainMenu -- update chars
  4 -> initMainMenu -- update shape
  _ -> initMainMenu -- Impossible State
  
-- Args: max value, default value, char
keyToInt :: Int -> Int -> Char -> Int
keyToInt m n c = if (k > 0) && (k <= m) then k else n
  where
    k = fromEnum c - fromEnum '0'