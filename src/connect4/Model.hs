{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude
import qualified Network.WebSockets as WS

import qualified Model.Board  as Board

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick String

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data GlobalState = GS
  { state    :: State
  , conn     :: WS.Connection
  , setting  :: SettingsList
  }

data State 
  = MainMenu Int
  | Loading
  | Instructions
  | Play PlayState
  | EndMenu EndMenuState
  | Settings Int
  
data PlayState = PS
  { psR      :: Player   -- ^ player R info
  , psB      :: Player   -- ^ player B info
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.RB        -- ^ whose turn 
  , psCol    :: Int             -- ^ current cursor
  }

data Player = Local | Server
  deriving (Eq)

data EndMenuState = EMS
  { emRes    :: Int             -- ^ game result (change type?)
  , emSel    :: Int             -- ^ current cursor   
  } 

data SettingsList = SL
  { colorScheme :: Int
  , discChar    :: Int
  , discShape   :: Int
  }

initLocalGame :: State
initLocalGame = Play $ PS 
  { psR      = Local
  , psB      = Local
  , psBoard  = Board.init
  , psTurn   = Board.R
  , psCol    = (Board.width + 1) `div` 2
  }

initMultiplayerGame :: String -> State
initMultiplayerGame str = Play $ PS 
  { psR      = if str == "R" then Local else Server
  , psB      = if str == "B" then Local else Server
  , psBoard  = Board.init
  , psTurn   = Board.R
  , psCol    = (Board.width + 1) `div` 2
  }

initSettings :: State
initSettings = Settings 0

initMainMenu :: State
initMainMenu = MainMenu 0

initEndMenu :: Int -> State
initEndMenu n = EndMenu $ EMS
  { emRes    = n
  , emSel    = 0 
  }

initSettingsList :: SettingsList
initSettingsList = SL
  { colorScheme = 0
  , discChar    = 0
  , discShape   = 0
  }

mainMenuOptionCount :: Int
mainMenuOptionCount = 5

endMenuOptionCount :: Int
endMenuOptionCount = 3

settingsOptionCount :: Int
settingsOptionCount = 4

isCurr :: PlayState -> Int -> Bool
isCurr s c = (psCol s) == c
