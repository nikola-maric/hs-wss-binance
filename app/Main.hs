{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.Chan.Unagi.Bounded (OutChan, newChan, readChan)
import Control.Monad (forever)
import WebSockets.Binance.DepthStream (differentialDepthOf, partialBookDepthOf)
import WebSockets.Binance.Stream (combineWith)
import WebSockets.Binance.StreamClient (listenFor)
import WebSockets.Binance.Trade (tradeOf)
import WebSockets.Binance.Types

main :: IO ()
main = do
  (priceInChan, priceOutChan) <- newChan 10
  concurrently_
    ( listenFor
        ( partialBookDepthOf (TradingPair @"adausdt") Five OneSecond
            `combineWith` differentialDepthOf (TradingPair @"bnbbtc") HundredMilliseconds
            `combineWith` tradeOf (TradingPair @"bnbbtc")
        )
        priceInChan
    )
    (readPrice priceOutChan)

readPrice :: Show a => OutChan a -> IO ()
readPrice channel = forever $ do
  msg <- readChan channel
  print msg
