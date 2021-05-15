{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module WebSockets.Binance.StreamClient ( listenFor, fullPath ) where

import Wuss ( runSecureClient )
import Control.Monad (forever)
import Network.WebSockets (receiveDataMessage)
import Control.Concurrent.Chan.Unagi.Bounded (InChan, tryWriteChan)
import Network.Socket (PortNumber)
import Data.WorldPeace (OpenUnion)
import GHC.TypeLits (KnownNat, KnownSymbol)
import WebSockets.Binance.Stream (SizeOf, JoinStreamNames, StreamOf (parseMessage), RecievePayload, RawStreamResponse (RawMessage), streamPath, streamName)

listenFor :: (KnownNat (SizeOf s), KnownSymbol (JoinStreamNames s)) => StreamOf s a -> InChan (RecievePayload (OpenUnion a)) -> IO ()
listenFor strm channel =
    runSecureClient wsHost wsPort (wsPath strm) $ \connection -> do
        forever $ do
            receiveDataMessage connection >>= tryWriteChan channel . parseMessage strm . RawMessage

wsHost :: String
wsHost = "stream.binance.com"

wsPort :: PortNumber
wsPort = 9443

wsPath :: (KnownNat (SizeOf s), KnownSymbol (JoinStreamNames s)) => StreamOf s a -> String
wsPath strm = streamPath strm ++ streamName strm

fullPath :: (KnownNat (SizeOf s), KnownSymbol (JoinStreamNames s)) => StreamOf s a -> String
fullPath stream = "wss://" ++ wsHost ++ ":" ++ show wsPort ++ wsPath stream