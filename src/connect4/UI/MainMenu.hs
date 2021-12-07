module UI.MainMenu (view) where

import Brick

view :: Int -> [Widget String]
view n = [strWrap $ "main menu placeholder. press enter to select item." ++ show n]