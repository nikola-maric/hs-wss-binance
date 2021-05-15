{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WebSockets.Binance.AggregatedTrade where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)
import GHC.TypeLits (AppendSymbol)
import WebSockets.Binance.Stream (StreamOf, streamOf)
import WebSockets.Binance.Types
  ( StreamType (AggregatedTradeOf),
    TradingPair,
  )
import Data.Text (Text)

data AggregatedTradeResponse = AggregatedTradeResponse
  { atrEventType :: Text,
    atrEventTime :: Integer,
    atrPrice :: Float,
    atrQuantity :: Float,
    atrFirstTradeId :: Integer,
    atrLastTradeId :: Integer,
    atrBuyerMarketMaker :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON AggregatedTradeResponse where
  parseJSON = withObject "AggregatedTradeResponse" $ \v ->
    AggregatedTradeResponse <$> v .: "e"
      <*> v .: "E"
      <*> fmap (read @Float) (v .: "p")
      <*> fmap (read @Float) (v .: "q")
      <*> v .: "f"
      <*> v .: "l"
      <*> v .: "m"

aggregatedTradeOf ::
  TradingPair cName ->
  StreamOf
    '[StreamType (AppendSymbol cName "@aggTrade") AggregatedTradeResponse]
    '[AggregatedTradeResponse]
aggregatedTradeOf pair = streamOf @AggregatedTradeResponse (AggregatedTradeOf pair)
