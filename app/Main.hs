{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.Chan.Unagi.Bounded (OutChan, newChan, readChan)
import Control.Monad (forever)
import Data.WorldPeace (OpenUnion)
import WebSockets.Binance.AggregatedTrade (AggregatedTradeResponse)
import WebSockets.Binance.CandlestickData (CandlestickDataResponse)
import WebSockets.Binance.DepthStream (differentialDepthStreamOf, partialBookDepthStreamOf)
import WebSockets.Binance.Stream (combineWith)
import WebSockets.Binance.StreamClient (listenFor)
import WebSockets.Binance.Trade
import WebSockets.Binance.Types

main :: IO ()
main = do
  (priceInChan, priceOutChan) <- newChan 10
  concurrently_
    ( listenFor
        ( partialBookDepthStreamOf (TradingPair @"adausdt") Five OneSecond
            `combineWith` differentialDepthStreamOf (TradingPair @"bnbbtc") HundredMilliseconds
            `combineWith` tradingOf (TradingPair @"bnbbtc")
        )
        priceInChan
    )
    (readPrice priceOutChan)

readPrice :: Show a => OutChan a -> IO ()
readPrice channel = forever $ do
  msg <- readChan channel
  print msg
