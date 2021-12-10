module UI.Settings (view, nMenuOptions) where

import Brick
import Graphics.Vty
import UI.Resources
import Model
import Brick.Widgets.Center
import Brick.Widgets.Border

nMenuOptions :: Int
nMenuOptions = length settingsOptions

view :: SettingsList -> Int -> [Widget String]
view sl n = let (s1,s2,s3) = parseSettings sl in
  [name <=> instructions <=> hCenter (options n) <=> currSettings s1 s2 s3]

currSettings :: Int -> Int -> Int -> Widget String
currSettings n1 n2 n3 = buildSettingsBorder n1 $ vBox $ str <$>
  [ "Current Settings: "
  , "Your Color / Opponent Color: " ++
    settingsThemeOptions !! n1
  , "Token Character: " ++
    [settingsCharOptions !! n2]
  , "Token Shape: " ++
    settingsShapeOptions !! n3
  ]

buildSettingsBorder :: Int -> Widget String -> Widget String
buildSettingsBorder n1 w = hCenter $ border $ padRight (Pad p) $ padLeft (Pad 5) w
  where p = 15 - length (settingsThemeOptions !! n1)

instructions :: Widget String
instructions = padLeftRight 24 $ vBox $ str <$> split optionsControls newline

options :: Int -> Widget String
options n = padTopBottom 4 $ padLeftRight 40 $ vBox $ buildOptions n

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