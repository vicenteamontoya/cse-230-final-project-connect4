module UI.Settings (view, nMenuOptions) where

import Brick
import Graphics.Vty
import UI.Resources
import qualified Data.List as L
import Model

nMenuOptions :: Int
nMenuOptions = length options

options :: [String]
options =
  [ "Color Scheme"
  , "Disk Character"
  , "Disk Shape"
  ]

view :: SettingsList -> Int -> [Widget String]
view sl n = let s@[s1,s2,s3] = parseSettings sl in
  [L.foldl' (<=>) 
  (strWrap "Change Settings")
  (buildOptions n s)]

parseSettings :: SettingsList -> [Int]
parseSettings sl = [colorScheme sl, diskChar sl, diskShape sl]

buildOptions :: Int -> [Int] -> [Widget String]
buildOptions n s = [mkOption n i | i <- [0..(nMenuOptions-1)]]

mkOption :: Int -> Int -> Widget String
mkOption n i
  | n == i    = withCursor $ strWrap $ options !! i
  | otherwise = strWrap $ options !! i

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withBackColor` gray)