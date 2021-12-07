module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player

-------------------------------------------------------------------------------

control :: State -> BrickEvent n Tick -> EventM n (Next State)
control s (T.VtyEvent (V.EvKey V.KEsc _)) = halt s -- Esc to Quit Game anytime
control s@(Play p) ev = case ev of 
  -- AppEvent Tick                   -> continue s
  T.VtyEvent (V.EvKey V.KEnter _) -> nextGameS p =<< liftIO (play p)
  T.VtyEvent (V.EvKey V.KLeft _)  -> continue (Play (move left p))
  T.VtyEvent (V.EvKey V.KRight _) -> continue (Play (move right p))
  _                               -> continue s -- halt s
control s@(MainMenu n) ev = case ev of 
  T.VtyEvent (V.EvKey V.KEnter _)    -> if n == 5 then halt s else continue (mainMenuSelect n)
  T.VtyEvent (V.EvKey (V.KChar c) _) -> continue (MainMenu $ keyToInt mainMenuOptionCount n c)
  T.VtyEvent (V.EvKey V.KDown _)     -> continue (MainMenu ((n `mod` mainMenuOptionCount) + 1))
  T.VtyEvent (V.EvKey V.KUp _)       -> continue (MainMenu (((n - 2) `mod` mainMenuOptionCount) + 1))
  _                                  -> continue s -- halt s
control s@(EndMenu (EMS r n)) ev = case ev of 
  T.VtyEvent (V.EvKey V.KEnter _)    -> if n == 3 then halt s else continue (endMenuSelect n)
  T.VtyEvent (V.EvKey (V.KChar c) _) -> continue (EndMenu $ EMS r (keyToInt endMenuOptionCount n c))
  T.VtyEvent (V.EvKey V.KDown _)     -> continue (EndMenu $ EMS r ((n `mod` endMenuOptionCount) + 1))
  T.VtyEvent (V.EvKey V.KUp _)       -> continue (EndMenu $ EMS r (((n - 2) `mod` endMenuOptionCount) + 1))
  _                                  -> continue s -- halt s
control s@(Loading) ev = case ev of 
  _                                  -> continue s -- halt s
control s@(Instructions) ev = case ev of 
  T.VtyEvent (V.EvKey V.KLeft _)  -> continue initMainMenu
  _                               -> continue s -- halt s  
control s@(Settings n) ev = case ev of
  T.VtyEvent (V.EvKey V.KEnter _)    -> continue (settingsSelect n)
  T.VtyEvent (V.EvKey (V.KChar c) _) -> continue (Settings $ keyToInt settingsOptionCount n c)
  T.VtyEvent (V.EvKey V.KDown _)     -> continue (Settings ((n `mod` settingsOptionCount) + 1))
  T.VtyEvent (V.EvKey V.KUp _)       -> continue (Settings (((n - 2) `mod` settingsOptionCount) + 1))
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
nextGameS :: PlayState -> Result Board -> EventM n (Next State)
-------------------------------------------------------------------------------
nextGameS s r = case r of
  Retry  -> continue (Play s)
  Cont b -> continue (Play $ s {psBoard = b, psTurn = (flipRB $ psTurn s)})
  _      -> continue (initEndMenu 0) -- implement result to int

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
  2 -> initMainMenu -- TODO update setting 1
  3 -> initMainMenu -- update setting 2
  4 -> initMainMenu -- update setting 3
  5 -> initMainMenu -- update setting 4
  _ -> initMainMenu -- Impossible State
  
-- Args: max value, default value, char
keyToInt :: Int -> Int -> Char -> Int
keyToInt m n c = if (k > 0) && (k <= m) then k else n
  where
    k = fromEnum c - fromEnum '0'