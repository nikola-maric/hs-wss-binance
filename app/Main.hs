{-# LANGUAGE TypeApplications #-}

module Main where

import WebSockets.Binance.StreamClient (listenFor)
import Control.Concurrent.Chan.Unagi.Bounded (newChan, readChan, OutChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import WebSockets.Binance.Types
import WebSockets.Binance.AggregatedTrade (AggregatedTradeResponse)
import WebSockets.Binance.CandlestickData (CandlestickDataResponse)

main :: IO ()
main = do
    (priceInChan, priceOutChan) <- newChan 10
    concurrently_ (
        listenFor (
            combineStreams
                (streamOf @AggregatedTradeResponse (AggregatedTradeOf (createCurrency "adausdt")))
                (streamOf @CandlestickDataResponse (CandlestickDataOf (createCurrency "bnbbtc") OneMinute))
            ) priceInChan
        ) (readPrice priceOutChan)

readPrice :: (Show a) => OutChan a -> IO ()
readPrice channel = forever $ do
    msg <- readChan channel
    print msg
    threadDelay 100000