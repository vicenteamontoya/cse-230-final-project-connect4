module UI.Resources where

import Data.Char

import Brick
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

title :: String
title = "\
\ ▄▄·        ▐ ▄  ▐ ▄ ▄▄▄ . ▄▄· ▄▄▄▄▄    ·▄▄▄      ▄• ▄▌▄▄▄   \n\
\▐█ ▌▪▪     •█▌▐█•█▌▐█▀▄.▀·▐█ ▌▪•██      ▐▄▄·▪     █▪██▌▀▄ █·\n\
\██ ▄▄ ▄█▀▄ ▐█▐▐▌▐█▐▐▌▐▀▀▪▄██ ▄▄ ▐█.▪    ██▪  ▄█▀▄ █▌▐█▌▐▀▀▄ \n\
\▐███▌▐█▌.▐▌██▐█▌██▐█▌▐█▄▄▌▐███▌ ▐█▌·    ██▌.▐█▌.▐▌▐█▄█▌▐█•█▌\n\
\·▀▀▀  ▀█▄▀▪▀▀ █▪▀▀ █▪ ▀▀▀ ·▀▀▀  ▀▀▀     ▀▀▀  ▀█▄▀▪ ▀▀▀ .▀  ▀\n\
\" 

disc :: String
disc = "\
\    ****    \n\     
\ ********** \n\    
\************\n\  
\************\n\   
\ ********** \n\   
\    ****    \n\
\"

menuInstructions:: String
menuInstructions = "\
  \Press `enter` to select option. \n\
  \Press `up` or `down` keys to change selected option. \n\
  \Press `left` to return to previous menu. \n\
  \Press `esc` to quit.\n\
  \"


gameInstructions :: String
gameInstructions = "\
\ Connect Four is a two-player board game played on a grid with seven columns \n\
\ and six rows. First, each player picks a color. On each player's turn, the  \n\
\ player chooses a column to drop a disc of their color. The disc will drop to\n\
\ the lowest available row within the column. The objective of the game is to \n\
\ be the first player to form a horizontal, vertical, or diagonal line of four\n\
\ of one's own discs. \n\
\"

gameControls :: String
gameControls = "\
  \Press `enter` to select a column and drop a disc of your color. \n\
  \Press `left` or `right` keys to change selected column. \n\
  \Press `esc` to quit.\n\
  \"

endGameOptions :: [String]
endGameOptions =
  [ "Main Menu"
  , "Settings"
  , "Quit"
  ]

mainMenuOptions :: [String]
mainMenuOptions =
  [ "Local Game"
  , "Multiplayer Game"
  , "Instructions"
  , "Settings"
  , "Quit"
  ]

newline :: Char
newline = chr 10

cellHeight :: Int
cellHeight = 6

drawResult :: String
drawResult = "There was a draw!"

winResult :: String
winResult = " won the game!"
-------------------------------------------------------------------------------
-- Helper methods
-------------------------------------------------------------------------------

split :: String -> Char -> [String]
split s c = if length cleanSuffix > 0 then [prefix] ++ (split cleanSuffix c) else [prefix]
  where
    (prefix, suffix) = break (== c) s
    cleanSuffix      = tail suffix 
    
-------------------------------------------------------------------------------
-- Attributes
-------------------------------------------------------------------------------

theMap :: AttrMap
theMap = attrMap
  defAttr
  [
   (redAttr, defAttr `withForeColor` red),
   (blueAttr, defAttr `withForeColor` darkBlue),
   (greenAttr, defAttr `withForeColor` green),
   (yellowAttr, defAttr `withForeColor` yellow),
   (magentaAttr, defAttr `withForeColor` magenta),
   (cyanAttr, defAttr `withForeColor` cyan)
  ]

gray :: Color
gray = rgbColor 50 50 50

darkBlue :: Color
darkBlue = rgbColor 71 105 242

redAttr, blueAttr, greenAttr,yellowAttr, magentaAttr,cyanAttr :: AttrName
redAttr     = attrName "Red"
blueAttr    = attrName "Blue"
greenAttr   = attrName "Green"
yellowAttr  = attrName "Yellow"
magentaAttr = attrName "Magenta"
cyanAttr    = attrName "Cyan"

getAttr :: Int -> (AttrName, AttrName) 
getAttr 0 = (redAttr, blueAttr)
getAttr 1 = (redAttr, yellowAttr)
getAttr 2 = (greenAttr, yellowAttr)
getAttr 3 = (magentaAttr, cyanAttr)
getAttr _ = (redAttr, blueAttr)

getAttrString :: Int -> (String, String) 
getAttrString 0 = ("Red", "Blue")
getAttrString 1 = ("Red", "Yellow")
getAttrString 2 = ("Green", "Yellow")
getAttrString 3 = ("Magenta", "Cyan")
getAttrString _ = ("Red", "Blue")