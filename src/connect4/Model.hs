{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = MainMenu 
  | Play PlayState 
  | EndMenu 
  
data PlayState = PS
  { psR      :: Player.Player   -- ^ player R info
  , psB      :: Player.Player   -- ^ player B info
  , psScore  :: Score.Score     -- ^ current score
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.RB        -- ^ whose turn 
  , psCol    :: Int             -- ^ current cursor
  , psResult :: Board.Result () -- ^ result      
  } 

init :: Int -> PlayState
init n = PS 
  { psR      = Player.human
  , psB      = Player.human
  , psScore  = Score.init n
  , psBoard  = Board.init
  , psTurn   = Board.R
  , psCol    = 1
  , psResult = Board.Cont ()
  }

isCurr :: PlayState -> Int -> Bool
isCurr s c = (psCol s) == c

next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
next s Board.Retry     = Right s
next s (Board.Cont b') = Right (s { psBoard = b'
                                  , psTurn  = Board.flipRB (psTurn s) })
next s res             = nextBoard s res 

nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
nextBoard s res = case res' of
                    Board.Win _ -> Left res' 
                    Board.Draw  -> Left res'
                    _           -> Right s' 
  where 
    sc'  = Score.add (psScore s) (Board.boardWinner res) 
    res' = Score.winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = mempty                -- clear the board
             , psTurn  = Score.startPlayer sc' -- toggle start player
             } 

