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
control s@(Play p) ev = case ev of 
  AppEvent Tick                   -> Brick.continue s
  T.VtyEvent (V.EvKey V.KEnter _) -> nextGameS p =<< liftIO (play p)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (Play (move left p))
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (Play (move right p))
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s
control MainMenu ev = case ev of 
  T.VtyEvent (V.EvKey V.KEnter _) -> Brick.continue (Play $ Model.init 3)
  _                               -> Brick.continue MainMenu -- Brick.halt s
control s _ = Brick.continue s

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
nextGameS s b = case next s b of
  Right s' -> continue (Play s')
  Left res -> halt (Play $ s { psResult = res }) 


