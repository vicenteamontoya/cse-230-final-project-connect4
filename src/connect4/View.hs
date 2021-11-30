module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
view :: State -> [Widget String]
-------------------------------------------------------------------------------
view s = case s of
  Play p -> [viewGame p]
  MainMenu -> mainMenuView
  _ -> []

mainMenuView :: [Widget String]
mainMenuView = [strWrap "main menu demo"]

viewGame :: PlayState -> Widget String
viewGame s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vTile [ mkRow s row | row <- [1..dim] ]

header :: PlayState -> String
header s = printf "Connect4 Turn = %s, col = %d" (show (psTurn s)) (psCol s)

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s c   = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkXO xoMb)
  where 
    xoMb      = psBoard s ! Pos r c
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkXO :: Maybe RB -> Widget n
mkXO Nothing  = blockB
mkXO (Just R) = blockRed
mkXO (Just B) = blockBlue

blockB, blockRed, blockBlue :: Widget n
blockB = vBox (replicate 5 (str "     "))
blockRed  = vBox [ str "X   X"
                 , str " X X "
                 , str "  X  "
                 , str " X X " 
                 , str "X   X"]
blockBlue = vBox [ str "OOOOO"
                 , str "O   O"
                 , str "O   O"
                 , str "O   O"
                 , str "OOOOO"]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget