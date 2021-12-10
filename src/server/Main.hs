{-# LANGUAGE OverloadedStrings #-}
module Main where
-- import Data.Char (isPunctuation, isSpace)
-- import Data.Monoid (mappend)
import Data.Text (Text, pack)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import System.Random (randomIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

-- Client: (User ID, Connection Object)
type Client = (Int, WS.Connection)
-- Server State: (List of Clients, State of Game)
type ServerState = ([Client], Int)

-- States of Game: 0 -> No ready players, 1 -> 1 ready player, 2 -> Player 1 turn, 3 -> Player 2 turn
newServerState :: ServerState
newServerState = ([], 0)

-- Utilities for operating on Server State
numClients :: ServerState -> Int
numClients = length . fst

addClient :: Client -> ServerState -> ServerState
addClient client clients = (client : (fst clients), snd clients)

-- If client is removed, game goes to no ready player state
removeClient :: Int -> ServerState -> ServerState
removeClient uid clients = (filter ((/= uid) . fst) (fst clients), if (snd clients) > 1 then 0 else (snd clients))

posClient :: Int -> ServerState -> Int
posClient uid clients = if fst (head (fst clients)) == uid then 0 else 1

getClient :: Int -> ServerState -> WS.Connection
getClient pos clients = snd ((fst clients) !! pos)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn ("Broadcast " <> message)
    forM_ (fst clients) $ \(_, conn) -> WS.sendTextData conn message

-- Main function
main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

-- Serve incoming connections
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        -- Check capacity
        s <- readMVar state
        if numClients s >= 2 then do
            T.putStrLn "Rejected Connection. Over Capacity"
            return ()
        else do
            -- Add client
            uid <- randomIO :: IO Int
            T.putStrLn ("Connected to user: " <> pack (show uid))
            flip finally (disconnect uid) $ do
                modifyMVar_ state $ \s ->
                    let s' = addClient (uid, conn) s in return s'
                -- Serve client indefinitely
                talk (uid, conn) state
                where
                   -- Handle disconnect: Remove from client list and broadcast end of game
                   disconnect uid = do
                       T.putStrLn ("Disconnect from user: " <> pack (show uid))
                       s <- readMVar state
                       modifyMVar_ state $ \s ->
                           let s' = removeClient uid s in return s'
                       s' <- readMVar state
                       if (snd s) > 1 then
                           broadcast "LEFT" s'
                       else
                           return ()

-- Serve client indefinitely
talk :: (Int, WS.Connection) -> MVar ServerState -> IO ()
talk (uid, conn) state = forever $ do
    msg <- WS.receiveData conn :: IO Text
    -- If message is "PLAY"
    if msg == "PLAY" then do
        T.putStrLn ("Received PLAY: " <> pack (show uid))
        s <- readMVar state
        if (snd s) < 2 then do
            modifyMVar_ state $ \s ->
                let s' = (fst s, ((snd s) + 1)) in return s'
        else do
            modifyMVar_ state $ \s ->
                let s' = (fst s, 1) in return s'
        s <- readMVar state
        -- If game is ready to be played
        if (snd s) == 2 then do
            T.putStrLn "Starting Game"
            let p1 = getClient 0 s
            let p2 = getClient 1 s
            WS.sendTextData p2 (pack "B")
            WS.sendTextData p1 (pack "R")
        else
            return ()
    -- If message is a move
    else do  
        T.putStrLn ("Move " <> msg <> ": " <> pack (show uid))
        s <- readMVar state
        let pos = (posClient uid s) + 2
        let opp = ((pos - 2) + 1) `mod` 2
        if (snd s) == pos then do
            WS.sendTextData (getClient opp s) msg
            modifyMVar_ state $ \s ->
                let s' = (fst s, (opp + 2)) in return s'
        else 
            return ()

