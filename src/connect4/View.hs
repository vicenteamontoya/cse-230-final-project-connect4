module View (view) where

import Brick

import Model
import qualified UI.MainMenu as M
import qualified UI.Loading as L
import qualified UI.Instructions as I
import qualified UI.Play as P
import qualified UI.EndMenu as E

-------------------------------------------------------------------------------
view :: State -> [Widget String]
-------------------------------------------------------------------------------
view s = case s of
  Play p -> P.view p
  MainMenu n -> M.view n
  Loading -> L.view
  Instructions -> I.view
  EndMenu e -> E.view e
