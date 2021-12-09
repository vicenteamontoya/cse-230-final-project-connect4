module UI.EndMenu (view) where

import Brick
import Model
import Graphics.Vty
import UI.Resources
import qualified Data.List as L

view :: EndMenuState -> [Widget String]
view n = let winString = mkWinString $ emRes n in
  [L.foldl' (<=>) (strWrap winString) (buildOptions $ emSel n)]

mkWinString :: Int -> String
mkWinString 0 = "There was a draw! "
mkWinString 1 = "Red won the game! "
mkWinString 2 = "Blue won the game! "
mkWinString n = error $ "undefined option: `" ++ show n ++ "` in EndMenu.hs:mkWinString"

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withBackColor` gray)

nMenuOptions :: Int
nMenuOptions = length options

options :: [String]
options =
  [ "Main Menu"
  , "Settings"
  , "Quit"
  ]

buildOptions :: Int -> [Widget String]
buildOptions n = [mkOption n i | i <- [0..(nMenuOptions-1)]]

mkOption :: Int -> Int -> Widget String
mkOption n i
  | n == i    = withCursor $ strWrap $ options !! i
  | otherwise = strWrap $ options !! i