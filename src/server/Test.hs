{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text (Text, pack)
import Control.Concurrent (forkIO, threadDelay)
import System.Random (randomIO)
import Network.Socket (withSocketsDo)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

main :: IO ()
main = do
    T.putStrLn "Basic Test Start"
    basicTest
    threadDelay 2000000 -- Let all threads from previous test end
    T.putStrLn "\nLeave Test Start"
    leaveTest
    threadDelay 2000000
    T.putStrLn "\nTurn Test Start"
    turnTest
    threadDelay 2000000

-- Back and forth communication b/w player 1 and player 2
basicTest :: IO ()
basicTest = do
    withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" $ (\conn -> do
        WS.sendTextData conn (pack "PLAY")
        -- Player 1
        forkIO $ withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" $ (\conn -> do
            WS.sendTextData conn (pack "PLAY")
            WS.receiveData conn :: IO Text
            WS.sendTextData conn (pack "1")
            msg <- WS.receiveData conn :: IO Text
            if msg == "2" then
                T.putStrLn "Basic Test Passes"
            else
                T.putStrLn ("Basic Test Failed: " <> msg))
        -- Player 2
        WS.receiveData conn :: IO Text
        msg <- WS.receiveData conn :: IO Text
        WS.sendTextData conn (pack "2"))

-- Player 1 Leaves in the middle
leaveTest :: IO ()
leaveTest = do
    withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" $ (\conn -> do
        WS.sendTextData conn (pack "PLAY")
        -- Player 1
        forkIO $ withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" $ (\conn -> do
            WS.sendTextData conn (pack "PLAY")
            WS.receiveData conn :: IO Text
            return ())
        -- Player 2
        WS.receiveData conn :: IO Text
        msg <- WS.receiveData conn
        if msg == "LEFT" then
            T.putStrLn "Leave Test Passes"
        else
            T.putStrLn ("Leave Test Failed: " <> msg))

-- Player 2 plays out of turn
turnTest :: IO ()
turnTest = do
    withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" $ (\conn -> do
        WS.sendTextData conn (pack "PLAY")
        -- Player 1
        forkIO $ withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" $ (\conn -> do
            WS.sendTextData conn (pack "PLAY")
            WS.receiveData conn :: IO Text
            threadDelay 2000000 -- Player 2 should go first
            WS.sendTextData conn (pack "1")
            msg <- WS.receiveData conn :: IO Text
            if msg == "1" then
                T.putStrLn "Turn Test Passes"
            else
                T.putStrLn ("Turn Test Failed: " <> msg))
        -- Player 2
        WS.receiveData conn :: IO Text
        WS.sendTextData conn (pack "2")
        msg <- WS.receiveData conn :: IO Text
        WS.sendTextData conn msg)
