module UI.Loading (view) where

import Brick

view :: [Widget String]
view = [strWrap "loading page placeholder. press esc to quit, press left to go back to main menu"]