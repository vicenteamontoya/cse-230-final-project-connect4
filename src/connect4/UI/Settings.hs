module UI.Settings (view, nMenuOptions) where

import Brick
import Graphics.Vty
import UI.Resources
import qualified Data.List as L
import Model
import Brick.Widgets.Center

nMenuOptions :: Int
nMenuOptions = length settingsOptions

view :: SettingsList -> Int -> [Widget String]
view sl n = let s@(s1,s2,s3) = parseSettings sl in
  [name <=> instructions <=> options n <=> currLabel <=> currSettings s1 s2 s3]

currSettings :: Int -> Int -> Int -> Widget String
currSettings = error "not implemented"

currLabel :: Widget String
currLabel = str "Current Settings:"

instructions :: Widget String
instructions = strWrap "Change Settings"

options :: Int -> Widget String
options n = padLeftRight 24 $ vBox $ buildOptions n

{- 
  (buildOptions n s)] -}

parseSettings :: SettingsList -> (Int, Int, Int)
parseSettings sl = (colorScheme sl, discChar sl, discShape sl)

buildOptions :: Int -> [Widget String]
buildOptions n = [mkOption n i | i <- [0..(nMenuOptions-1)]]

mkOption :: Int -> Int -> Widget String
mkOption n i
  | n == i    = withCursor $ strWrap $ settingsOptions !! i
  | otherwise = strWrap $ settingsOptions !! i

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withBackColor` gray)

name :: Widget n
name = padTopBottom 5 (hCenter (vBox (str <$> (split title newline))))