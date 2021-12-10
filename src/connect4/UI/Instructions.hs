module UI.Instructions (view) where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Graphics.Vty
import UI.Resources
import Brick.Types
import qualified Data.List as L

view :: [Widget String]
view = [v]
  where v = name <=> instructions <=> controlsH <=> controls

instructions :: Widget n
instructions = instructionsPadding (vBox (str <$> (split gameInstructions newline)))

instructionsPadding :: Widget n -> Widget n
instructionsPadding = padLeftRight 10

controls :: Widget n
controls = controlsPadding (vBox (str <$> (split gameControls newline)))

controlsH :: Widget n
controlsH = controlsPadding (str "Controls:")

controlsPadding :: Widget n -> Widget n
controlsPadding = padLeftRight 11 . padTop (Pad 1)

name :: Widget n
name = padTopBottom 5 (hCenter (vBox (str <$> (split title newline))))