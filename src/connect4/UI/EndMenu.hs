module UI.EndMenu (view) where

import Brick
import Model

view :: EndMenuState -> [Widget String]
view (EMS _ n) = [strWrap $ "end menu placeholder. press enter to select option" ++ show n]