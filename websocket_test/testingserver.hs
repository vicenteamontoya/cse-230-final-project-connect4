{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever, unless)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, forkIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Control.Monad.IO.Class (MonadIO(liftIO))

type Client = (Text, (WS.Connection, Bool))
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    putStrLn "Connected!"

    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)
