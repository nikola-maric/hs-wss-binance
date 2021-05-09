{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module WebSockets.Binance.StreamClient ( listenFor, fullPath ) where

import Wuss ( runSecureClient )
import Control.Monad (forever)
import Network.WebSockets (receiveDataMessage)
import WebSockets.Binance.Types ( RecievePayload(..), StreamOf(..), streamPath, streamName, RawStreamResponse (RawMessage), JoinStreamNames )
import Control.Concurrent.Chan.Unagi.Bounded (InChan, tryWriteChan)
import Utils.Monad ( ifM )
import Network.Socket (PortNumber)
import Data.WorldPeace (OpenUnion)
import GHC.TypeLits (KnownNat, KnownSymbol)
import WebSockets.Binance.Types (SizeOf)

listenFor :: (KnownNat (SizeOf s), KnownSymbol (JoinStreamNames s)) => StreamOf s a -> InChan (OpenUnion a) -> IO ()
listenFor strm channel =
    runSecureClient wsHost wsPort (wsPath strm) $ \connection -> do
        putStrLn ("Connected to " ++ fullPath strm)
        forever $ do
            msg <- RawMessage <$> receiveDataMessage connection
            let parsedMsg = parseMessage strm msg
            case parsedMsg of
                Successfull msgVal -> ifM (tryWriteChan channel msgVal) (putStrLn "Added price to queue") (putStrLn "Queue full, skipping add..")
                Failed reason      -> putStrLn $ "Failed to parse message, reason: " ++ reason

wsHost :: String 
wsHost = "stream.binance.com"

wsPort :: PortNumber 
wsPort = 9443

wsPath :: (KnownNat (SizeOf s), KnownSymbol (JoinStreamNames s)) => StreamOf s a -> String 
wsPath strm = streamPath strm ++ streamName strm 

fullPath :: (KnownNat (SizeOf s), KnownSymbol (JoinStreamNames s)) => StreamOf s a -> String 
fullPath stream = "wss://" ++ wsHost ++ ":" ++ show wsPort ++ wsPath stream