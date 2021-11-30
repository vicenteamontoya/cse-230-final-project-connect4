{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board (Result (..), RB (..))

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scMax  :: Int  -- ^ total number of boards
  , scR    :: Int  -- ^ points for player R 
  , scB    :: Int  -- ^ points for player B 
  , scD    :: Int  -- ^ drawn games 
  }
  deriving (Eq, Ord, Show)

init :: Int -> Score
init n = Score n 0 0 0

add :: Score -> Maybe RB -> Score
add sc (Just R) = sc { scR = scR sc + 1 }
add sc (Just B) = sc { scB = scB sc + 1 }
add sc Nothing  = sc { scD = scD sc + 1 }

get :: Score -> RB -> Int
get Score {..} R = scR 
get Score {..} B = scB 

currRound :: Score -> Int
currRound Score {..} = scR + scB + scD + 1

startPlayer :: Score -> RB
startPlayer sc 
  | even (currRound sc) = R
  | otherwise           = B

winner :: Score -> Result () 
winner sc@Score {..}
  | scR > scB + left = Win R
  | scB > scR + left = Win B
  | left == 0        = Draw
  | otherwise        = Cont ()
  where 
    left             = 1 + scMax - currRound sc