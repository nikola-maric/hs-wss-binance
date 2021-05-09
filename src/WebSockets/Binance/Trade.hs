{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module WebSockets.Binance.Trade where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)

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