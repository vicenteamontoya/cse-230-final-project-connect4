module UI.MainMenu (view, nOptions) where

import Brick
import Graphics.Vty
import UI.Resources
import qualified Data.List as L

nOptions :: Int
nOptions = length options

options :: [String]
options =
  [ "Play Local Game"
  , "Join Multiplayer Game"
  , "Instructions"
  , "Settings"
  , "Quit"
  ]

buildOptions :: Int -> [Widget String]
buildOptions n = [mkOption n i | i <- [0..(nOptions-1)]]

mkOption :: Int -> Int -> Widget String
mkOption n i
  | n == i    = withCursor $ strWrap $ options !! i
  | otherwise = strWrap $ options !! i

view :: Int -> [Widget String]
view n = [L.foldl' (<=>) 
  (strWrap "Press `enter` to select item. \
  \ Press `up` or `down` keys to change selected option. ")
  (buildOptions n)]

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withBackColor` gray)