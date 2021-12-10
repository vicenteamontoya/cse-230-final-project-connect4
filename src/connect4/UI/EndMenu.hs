module UI.EndMenu (view) where

import Brick
import Brick.Widgets.Center (center, hCenter)
import Model
import Graphics.Vty
import UI.Resources
import qualified Data.List as L
 
view :: EndMenuState -> SettingsList -> [Widget String]
view state settings = [v]
   where v = name <=> (result (emRes state) (colorScheme settings) ) <=> (options $ emSel state)

result :: Int -> Int -> Widget n
result 0 _       = resultPadding (str drawResult)
result 1 scheme  = resultPadding (withAttr attribute (str (winningColor ++ winResult)) )
  where
    winningColor = fst (getAttrString scheme)
    attribute    = attrName winningColor
result 2 scheme  = resultPadding (withAttr attribute (str (winningColor ++ winResult)) )
  where
    winningColor = snd (getAttrString scheme)
    attribute    = attrName winningColor
result n _      = error $ "undefined option: `" ++ show n ++ "` in EndMenu.hs:mkWinString"

options:: Int -> Widget n
options n = optionsPadding (vBox (buildOptions n))

resultPadding :: Widget n -> Widget n
resultPadding = padLeftRight 40

optionsPadding :: Widget n -> Widget n
optionsPadding = padLeftRight 40 . padTopBottom 5

nMenuOptions :: Int
nMenuOptions = length endGameOptions

name :: Widget n
name = padTopBottom 5 (hCenter (vBox (str <$> (split title newline))))

buildOptions :: Int -> [Widget n]
buildOptions n = [(mkOption n i) | i <- [0..(nMenuOptions-1)]]

mkOption :: Int -> Int -> Widget n
mkOption n i
  | n == i    = withCursor $ strWrap $ endGameOptions !! i
  | otherwise = strWrap $ endGameOptions !! i

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withBackColor` gray)