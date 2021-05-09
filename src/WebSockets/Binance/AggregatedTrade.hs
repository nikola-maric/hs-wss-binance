{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module WebSockets.Binance.AggregatedTrade where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)

data AggregatedTradeResponse = AggregatedTradeResponse {
        atrEventType :: String,
        atrEventTime :: Integer,
        atrPrice     :: Float,
        atrQuantity  :: Float,
        atrFirstTradeId  :: Integer,
        atrLastTradeId  :: Integer,
        atrBuyerMarketMaker :: Bool
    } deriving stock (Eq, Show, Generic)
      deriving anyclass ToJSON

instance FromJSON AggregatedTradeResponse where
    parseJSON = withObject "AggregatedTradeResponse" $ \v ->
        AggregatedTradeResponse <$> v .: "e"
            <*> v .: "E"
            <*> fmap (read @Float) (v .: "p")
            <*> fmap (read @Float) (v .: "q")
            <*> v .: "f"
            <*> v .: "l"
            <*> v .: "m"