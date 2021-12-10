{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Main where

import Test.Tasty
import Common
import Test.QuickCheck
import UI.Resources
import Model.Board
import qualified Data.Map as M

main :: IO ()
main = runTests
  [ manualTests
  , autoTests
  ]

manualTests ::  Score -> TestTree
manualTests sc = testGroup "Manual Tests"
  [ scoreTest ((\_ -> 1+1),  (), 2, 1, "test-add") -- verify that the manual tests are working
  , scoreTest ((\x -> split x '\n'),  "a\nb\nc\n", ["a","b","c"], 1, "test-split-helper-1")
  , scoreTest ((\x -> split x '$'),  "a$b$c$", ["a","b","c"], 1, "test-split-helper-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

autoTests :: Score -> TestTree
autoTests sc = testGroup "Automated Tests"
  [ scoreProp sc ("prop_win_board", prop_win_board, 1)
  ]

genRandCol :: Gen Int
genRandCol = elements [1..7]

prop_win_board :: Property
prop_win_board = forAll genRandWinBoard hasWinR

-- designed for only checking wins for R, but this should be symmetric
-- as both R and B use the same functions
hasWinR :: (Pos, Board) -> Bool
hasWinR (p,b) = wins b p R 

-- These functions all manually populate specific locations in the Board in order
-- to avoid having to populate all preceding entries while avoiding a preemtive win
-- condition. Each function only sets exactly 4 entries to trigger a win condition.
genRandWinBoard, genRandWinHorizontal, genRandWinVertical, genRandWinDiagonalUp, genRandWinDiagonalDown :: Gen (Pos, Board)
genRandWinBoard = do {
  n <- elements [1..4];
  case n of
    1 -> genRandWinHorizontal;
    2 -> genRandWinVertical;
    3 -> genRandWinDiagonalUp;
    _ -> genRandWinDiagonalDown;
}

-- generates a random board with a winning horizontal configuration
genRandWinHorizontal = do {
  sCol <- elements [1..4];
  r    <- elements [1..6];
  let b = M.insert (Pos r sCol) R Model.Board.init in
    let b' = M.insert (Pos r (sCol + 1)) R b in
      let b'' = M.insert (Pos r (sCol + 2)) R b' in
        let b''' = M.insert (Pos r (sCol + 3)) R b'' in
  return (Pos r (sCol + 3), b''')
}

-- generates a random board with a winning vertical configuration
genRandWinVertical = do {
  sRow <- elements [1..3];
  c    <- elements [1..7];
  let b = M.insert (Pos sRow c) R Model.Board.init in
    let b' = M.insert (Pos (sRow + 1) c) R b in
      let b'' = M.insert (Pos (sRow + 2) c) R b' in
        let b''' = M.insert (Pos (sRow + 3) c) R b'' in
  return (Pos (sRow + 3) c, b''')
}

-- generates a random board with a winning diagonal up configuration
-- (diagonal up meaning going up and to the right)
genRandWinDiagonalUp = do {
  sRow <- elements [4..7];
  sCol <- elements [1..4];
  let b = M.insert (Pos sRow sCol) R Model.Board.init in
    let b' = M.insert (Pos (sRow - 1) (sCol + 1)) R b in
      let b'' = M.insert (Pos (sRow - 2) (sCol + 2)) R b' in
        let b''' = M.insert (Pos (sRow - 3) (sCol + 3)) R b'' in
  return (Pos (sRow - 3) (sCol + 3), b''')
}

-- generates a random board with a winning diagonal down configuration
genRandWinDiagonalDown = do {
  sRow <- elements [1..3];
  sCol <- elements [1..4];
  let b = M.insert (Pos sRow sCol) R Model.Board.init in
    let b' = M.insert (Pos (sRow + 1) (sCol + 1)) R b in
      let b'' = M.insert (Pos (sRow + 2) (sCol + 2)) R b' in
        let b''' = M.insert (Pos (sRow + 3) (sCol + 3)) R b'' in
  return (Pos (sRow + 3) (sCol + 3), b''')
}

-------------------------------------------------------------------------------
-- Non-exported functions copied over from `Board.hs` to test win conditions --
-------------------------------------------------------------------------------

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