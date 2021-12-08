{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type Client = (Text, (WS.Connection, Bool))
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Text -> ServerState -> ServerState
removeClient user = filter ((/= user) . fst)

getTurn :: Text -> ServerState -> Bool
getTurn user = snd . snd . head . filter ((== user) . fst)

updateTurn :: ServerState -> ServerState
updateTurn = map (\(user, (conn, turn)) -> (user, (conn, not turn)))

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, (conn, _)) -> WS.sendTextData conn message

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        msg <- WS.receiveData conn
        clients <- readMVar state
        flip finally (disconnect msg) $ do
            modifyMVar_ state $ \s -> do
                if numClients s == 0 then do
                    let s' = addClient (msg, (conn, True)) s
                    WS.sendTextData conn $ "Welcome " <> msg
                    return s'
                else if numClients s == 1 then do
                    let s' = addClient (msg, (conn, False)) s
                    WS.sendTextData conn $ "Welcome " <> msg
                    broadcast "2 Clients Joined" s'
                    return s'
                else    
                    return s
            talk (msg, conn) state
                where
                   disconnect msg = do
                       s <- modifyMVar state $ \s ->
                           let s' = removeClient msg s in return (s', s')
                       broadcast (msg <> " disconnected") s

talk :: (Text, WS.Connection) -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    s <- readMVar state
    let turn = getTurn user s
    -- if (turn && (numClients s == 2)) then do
    if turn then do
        modifyMVar_ state $ \s ->
            let s' = updateTurn s in return s'
        s <- readMVar state
        broadcast (user `mappend` ": " `mappend` msg) s
    else
        return ()

