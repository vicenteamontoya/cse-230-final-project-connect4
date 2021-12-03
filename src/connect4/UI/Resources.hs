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
-- disc = "\
-- \    ████    \n\
-- \ ██████████ \n\
-- \████████████\n\
-- \████████████\n\
-- \ ██████████ \n\
-- \    ████    \n\
-- \"

disc = "\
\    ****    \n\     
\ ********** \n\    
\************\n\  
\************\n\   
\ ********** \n\   
\    ****    \n\
\"

newline :: Char
newline = chr 10

cellHeight :: Int
cellHeight = 6
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
