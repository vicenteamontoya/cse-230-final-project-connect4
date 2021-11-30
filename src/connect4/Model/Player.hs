module Model.Player where

import Model.Board

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player 
  { plName  :: String 
  , plStrat :: Strategy
  } 

type Strategy = Int     -- ^ current selected column
             -> Board   -- ^ current board
             -> RB      -- ^ red disk or blue disk
             -> IO Int  -- ^ next move

human :: Player 
human = Player "human" (\p _ _ -> return p)
