{-# LANGUAGE DeriveFunctor #-}
module Model.Board
  ( -- * Types
    Board
  , RB (..)
  , Pos (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , positions
  , emptyPositions
  , boardWinner
  , flipRB

    -- * Moves
  , left
  , right
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M 

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Board = M.Map Pos RB

data RB 
  = R 
  | B
  deriving (Eq, Show)

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

instance Show Pos where
  show (Pos r c) = show (r,c) 

(!) :: Board -> Pos -> Maybe RB 
board ! pos = M.lookup pos board

dim :: Int
dim = 7

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ] 

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- positions, M.notMember p board]

init :: Board
init = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------
                 
data Result a 
  = Draw 
  | Win RB
  | Retry 
  | Cont a
  deriving (Eq, Functor, Show)

put :: Board -> RB -> Int -> Result Board
put board rb col = case M.lookup (Pos 1 col) board of 
  Just _  -> Retry
  Nothing -> result rb $ insert board col rb

-- takes in row, color, board state and returns pos and updated board
insert :: Board -> Int -> RB -> (Pos, Board)
insert b col rb = (p, M.insert p rb b)
  where p = Pos row col
        row = maximum [ i | i <- [1..dim], M.notMember (Pos i col) b ]

-- pass position to make check efficient
result :: RB -> (Pos, Board) -> Result Board
result rb (p, b) 
  | wins b p rb  = Win  rb
  | isFull b  = Draw
  | otherwise = Cont b

wins :: Board -> Pos -> RB -> Bool
wins b p rb = or [ foldl f True i | i <- winPositions p ]
  where f False _ = False
        f _ p     = case M.lookup p b of
          Just color -> (color == rb)
          Nothing    -> False

winPositions :: Pos -> [[Pos]]
winPositions p = [ [ Pos (pRow p + i + j) (pCol p) | j <- [0..3] ] | i <- [-3..0] ] ++
                 [ [ Pos (pRow p) (pCol p + i + j) | j <- [0..3] ] | i <- [-3..0] ] ++
                 [ [ Pos (pRow p + i + j) (pCol p + i + j) | j <- [0..3] ] | i <- [-3..0] ] ++
                 [ [ Pos (pRow p + i + j) (pCol p - i - j) | j <- [0..3] ] | i <- [-3..0] ]

-- Testing
-- >>> winPositions (Pos 0 0)
-- [[(-3,0),(-2,0),(-1,0),(0,0)],[(-2,0),(-1,0),(0,0),(1,0)],[(-1,0),(0,0),(1,0),(2,0)],[(0,0),(1,0),(2,0),(3,0)],[(0,-3),(0,-2),(0,-1),(0,0)],[(0,-2),(0,-1),(0,0),(0,1)],[(0,-1),(0,0),(0,1),(0,2)],[(0,0),(0,1),(0,2),(0,3)],[(-3,-3),(-2,-2),(-1,-1),(0,0)],[(-2,-2),(-1,-1),(0,0),(1,1)],[(-1,-1),(0,0),(1,1),(2,2)],[(0,0),(1,1),(2,2),(3,3)],[(-3,3),(-2,2),(-1,1),(0,0)],[(-2,2),(-1,1),(0,0),(1,-1)],[(-1,1),(0,0),(1,-1),(2,-2)],[(0,0),(1,-1),(2,-2),(3,-3)]]
--

isFull :: Board -> Bool
isFull b = M.size b == dim * dim

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

left :: Int -> Int 
left p = max 1 (p - 1) 

right :: Int -> Int 
right p = min dim (p + 1) 

boardWinner :: Result a -> Maybe RB
boardWinner (Win rb) = Just rb
boardWinner _        = Nothing

flipRB :: RB -> RB
flipRB R = B
flipRB B = R