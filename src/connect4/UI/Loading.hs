module UI.Loading (view) where

import Brick
import Brick.Widgets.Center
import UI.Resources

view :: [Widget String]
view = [v]
  where v = name <=> loading 

loading :: Widget n
loading = loadingPadding (vBox (str <$> (split loadingText newline)))

loadingPadding :: Widget n -> Widget n
loadingPadding = padLeftRight 35

name :: Widget n
name = padTopBottom 5 (hCenter (vBox (str <$> (split title newline))))