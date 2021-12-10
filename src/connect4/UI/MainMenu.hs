module UI.MainMenu (view, nMenuOptions) where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Graphics.Vty
import UI.Resources
import Brick.Types
import qualified Data.List as L

view :: Int -> [Widget String]
view n = [v]
  where v = name <=> instructions <=> (options n) 

options:: Int -> Widget n
options n = optionsPadding (vBox (buildOptions n))

instructions :: Widget n
instructions = instructionsPadding (vBox (str <$> (split menuInstructions newline)))

instructionsPadding :: Widget n -> Widget n
instructionsPadding = padLeftRight 24

optionsPadding :: Widget n -> Widget n
optionsPadding = padLeftRight 40 . padTopBottom 5

nMenuOptions :: Int
nMenuOptions = length mainMenuOptions

name :: Widget n
name = padTopBottom 5 (hCenter (vBox (str <$> (split title newline))))

buildOptions :: Int -> [Widget n]
buildOptions n = [(mkOption n i) | i <- [0..(nMenuOptions-1)]]

mkOption :: Int -> Int -> Widget n
mkOption n i
  | n == i    = withCursor $ strWrap $ mainMenuOptions !! i
  | otherwise = strWrap $ mainMenuOptions !! i

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withBackColor` gray)