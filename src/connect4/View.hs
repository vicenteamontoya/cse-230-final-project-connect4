module View (view) where

import Brick

import Model
import qualified UI.MainMenu as M
import qualified UI.Loading as L
import qualified UI.Instructions as I
import qualified UI.Play as P
import qualified UI.EndMenu as E

-------------------------------------------------------------------------------
view :: GlobalState -> [Widget String]
-------------------------------------------------------------------------------
view gs@(GS st _ sl) = case st of
  Play p -> P.view p
  MainMenu n -> M.view n
  Loading -> L.view
  Instructions -> I.view
  EndMenu e -> E.view e
  Settings n -> [strWrap $ "settings placeholder. press enter to select option" ++ (show n)]