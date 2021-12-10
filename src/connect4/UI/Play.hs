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
view s sl = [v]
  where v = name <=> (withBorderStyle unicodeBold $
              borderWithLabel (header s) $
              vTile ((vLimit cellHeight) <$> ([ mkRow s row sl | row <- [1..height] ])))

header :: PlayState -> Widget n
header s =  str (printf "Connect4 Turn = %s, col = %d" (show (psTurn s)) (psCol s))

name :: Widget n
name = center (vBox (str <$> (split title newline)))

mkRow :: PlayState -> Int -> SettingsList -> Widget n
mkRow s row sl = hTile [ mkCell s row i sl | i <- [1..width] ]

mkCell :: PlayState -> Int -> Int -> SettingsList -> Widget n
mkCell s r c sl
  | isCurr s c   = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c sl

mkCell' :: PlayState -> Int -> Int -> SettingsList -> Widget n
mkCell' s r c sl = center (mkDisc disc sl)
  where 
    disc         = psBoard s ! Pos r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withBackColor` gray)

mkDisc :: Maybe RB -> SettingsList -> Widget n
mkDisc Nothing  = discBlank
mkDisc (Just R) = discRed
mkDisc (Just B) = discBlue

discBlank, discRed, discBlue :: SettingsList -> Widget n
discBlank _  = vBox (replicate 6 (str "     "))
discRed   sl = withAttr c1 (vBox (str <$> (split discStr newline)))
  where 
    (c1,_) = getAttr $ colorScheme sl
    char    = settingsCharOptions !! discChar sl
    discStr = (shapeFunctions !! discShape sl) char
discBlue  sl = withAttr c2 (vBox (str <$> (split discStr newline)))
  where 
    (_,c2) = getAttr $ colorScheme sl
    char    = settingsCharOptions !! discChar sl
    discStr = (shapeFunctions !! discShape sl) char

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget
