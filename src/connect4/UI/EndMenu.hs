module UI.EndMenu (view) where

import Brick
import Model

view :: EndMenuState -> [Widget String]
view _ = [strWrap "end menu placeholder. press enter to select option"]