module View (view, colorSchemeCount, diskCharCount, diskShapeCount) where

import Brick

import Model
import qualified UI.MainMenu as M
import qualified UI.Loading as L
import qualified UI.Instructions as I
import qualified UI.Play as P
import qualified UI.EndMenu as E
import qualified UI.Settings as S

-------------------------------------------------------------------------------
view :: GlobalState -> [Widget String]
-------------------------------------------------------------------------------
view gs@(GS st _ sl) = case st of
  Play p -> P.view p sl
  MainMenu n -> M.view n
  Loading -> L.view
  Instructions -> I.view
  EndMenu e -> E.view e
  Settings n -> S.view sl n

colorSchemeCount :: Int
colorSchemeCount = 5

diskCharCount :: Int
diskCharCount = 5

diskShapeCount :: Int
diskShapeCount = 5