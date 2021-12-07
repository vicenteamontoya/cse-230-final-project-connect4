{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Player as Player

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = MainMenu Int
  | Loading
  | Instructions
  | Play PlayState
  | EndMenu EndMenuState
  | Settings Int
  
data PlayState = PS
  { psR      :: Player.Player   -- ^ player R info
  , psB      :: Player.Player   -- ^ player B info
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.RB        -- ^ whose turn 
  , psCol    :: Int             -- ^ current cursor
  }

data EndMenuState = EMS
  { emRes    :: Int             -- ^ game result (change type?)
  , emSel    :: Int             -- ^ current cursor   
  } 

initGame :: State
initGame = Play $ PS 
  { psR      = Player.human
  , psB      = Player.human
  , psBoard  = Board.init
  , psTurn   = Board.R
  , psCol    = (Board.width + 1) `div` 2
  }

initSettings :: State
initSettings = Settings 1

initMainMenu :: State
initMainMenu = MainMenu 1

initEndMenu :: Int -> State
initEndMenu n = EndMenu $ EMS
  { emRes    = n
  , emSel    = 1 
  } 

mainMenuOptionCount :: Int
mainMenuOptionCount = 5

endMenuOptionCount :: Int
endMenuOptionCount = 3

settingsOptionCount :: Int
settingsOptionCount = 5 --TODO

isCurr :: PlayState -> Int -> Bool
isCurr s c = (psCol s) == c
