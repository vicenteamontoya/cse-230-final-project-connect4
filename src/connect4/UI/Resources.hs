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
  \Press `enter` to select item. \n\
  \Press `up` or `down` keys to change selected option. \n\
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