{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module WebSockets.Binance.StreamClient (listenFor, fullPath) where

import Control.Concurrent.Chan.Unagi.Bounded (InChan, tryWriteChan)
import Control.Monad (forever)
import Data.WorldPeace (OpenUnion)
import GHC.TypeLits (KnownNat, KnownSymbol)
import Network.Socket (PortNumber)
import Network.WebSockets (receiveDataMessage)
import WebSockets.Binance.Stream (JoinStreamNames, RawStreamResponse (RawMessage), RecievePayload, SizeOf, StreamOf (parseMessage), streamName, streamPath)
import Wuss (runSecureClient)

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
