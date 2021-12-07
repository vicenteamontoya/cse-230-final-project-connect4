module Main where

import Brick
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Network.Socket (withSocketsDo)
import Data.Text
import qualified Network.WebSockets  as WS

import Model
import View 
import Control 
import UI.Resources

-------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" app --TODO update host, port, path

app :: WS.ClientApp ()
app conn = do
    chan   <- newBChan 10
    forkIO  $ forever $ do
      msg <- WS.receiveData conn
      putStrLn (unpack msg)
      writeBChan chan Tick

    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    customMain initialVty buildVty (Just chan) brickApp $ GS initMainMenu conn initSettingsList
    WS.sendClose conn ((pack "BYE") :: Text)

brickApp :: App GlobalState Tick String
brickApp = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }
