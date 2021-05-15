{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
module WebSockets.Binance.Trade where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)
import GHC.TypeLits (AppendSymbol)
import WebSockets.Binance.Types
    ( TradingPair, StreamType(TradingOf) )
import WebSockets.Binance.Stream (StreamOf, streamOf)

data TradeResponse = TradeResponse {
        trEventType        :: String,
        trEventTime        :: Integer,
        trEventSymbol      :: String,
        trTradeId          :: Integer,
        trPrice            :: Float,
        trQuantity         :: Float,
        trBuyerOrderId     :: Integer,
        trSellerOrderId    :: Integer,
        trTradeTime        :: Integer,
        trBuyerMarkerMaker :: Bool
    } deriving stock (Eq, Show, Generic)
      deriving anyclass ToJSON

instance FromJSON TradeResponse where
    parseJSON = withObject "TradeResponse" $ \v ->
        TradeResponse <$> v .: "e"
            <*> v .: "E"
            <*> v .: "s"
            <*> v .: "t"
            <*> fmap (read @Float) (v .: "p")
            <*> fmap (read @Float) (v .: "q")
            <*> v .: "b"
            <*> v .: "a"
            <*> v .: "T"
            <*> v .: "m"

tradingOf :: TradingPair cName -> StreamOf
     '[StreamType (AppendSymbol cName "@trade") TradeResponse]
     '[TradeResponse]
tradingOf pair = streamOf @TradeResponse (TradingOf pair)