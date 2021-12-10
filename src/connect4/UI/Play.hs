module UI.Play (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style
import Text.Printf (printf)

import Model
import Model.Board
import Graphics.Vty hiding (dim)

import UI.Resources

view :: PlayState -> SettingsList -> [Widget String]
view s _ = [v]
  where v = name <=> (withBorderStyle unicodeBold $
              borderWithLabel (header s) $
              vTile ((vLimit cellHeight) <$> ([ mkRow s row | row <- [1..height] ])))

header :: PlayState -> Widget n
header s =  str (printf "Connect4 Turn = %s, col = %d" (show (psTurn s)) (psCol s))

name :: Widget n
name = center (vBox (str <$> (split title newline)))

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..width] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s c   = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

mkCell' :: PlayState -> Int -> Int -> Widget n
mkCell' s r c = center (mkDisc disc)
  where 
    disc      = psBoard s ! Pos r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withBackColor` gray)

mkDisc :: Maybe RB -> Widget n
mkDisc Nothing  = discBlank
mkDisc (Just R) = discRed
mkDisc (Just B) = discBlue

discBlank, discRed, discBlue :: Widget n
discBlank = vBox (replicate 6 (str "     "))
discRed   = withAttr redAttr (vBox (str <$> (split disc newline)))
discBlue  = withAttr blueAttr (vBox (str <$> (split disc newline)))

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget
