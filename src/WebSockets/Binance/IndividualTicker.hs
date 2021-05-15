{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module WebSockets.Binance.IndividualTicker where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)
import WebSockets.Binance.Stream (streamOf, StreamOf)
import WebSockets.Binance.Types (StreamType (IndividualTickerOf), TradingPair)
import GHC.TypeLits (AppendSymbol)
import Data.Text (Text)

data IndividualTickerResponse = IndividualTickerResponse
  { itrEventType :: Text,
    itrEventTime :: Integer,
    itrSymbol :: Text,
    itrPriceChange :: Float,
    itrPriceChangePercent :: Float,
    itrWeightedAveragePrice :: Float,
    itrFirstTradeBeforeWindowPrice :: Float,
    itrLastPrice :: Float,
    itrLastQuantity :: Float,
    itrBestBidPrice :: Float,
    itrBestBidQuantity :: Float,
    itrBestAskPrice :: Float,
    itrBestAskQuantity :: Float,
    itrOpenPrice :: Float,
    itrHighPrice :: Float,
    itrLowPrice :: Float,
    itrTotalTradedBaseAssetVolume :: Float,
    itrTotalTradedQuoteAssetVolume :: Float,
    itrStatisticsOpenTime :: Integer,
    itrStatisticsCloseTime :: Integer,
    itrFirstTradeId :: Integer,
    itrLastTradeId :: Integer,
    itrTotalNumberOfTrades :: Integer

  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON IndividualTickerResponse where
    parseJSON = withObject "IndividualTickerResponse" $ \v ->
        IndividualTickerResponse <$> v .: "e"
            <*> v .: "E"
            <*> v .: "s"
            <*> fmap (read @Float) (v .: "p")
            <*> fmap (read @Float) (v .: "P")
            <*> fmap (read @Float) (v .: "w")
            <*> fmap (read @Float) (v .: "x")
            <*> fmap (read @Float) (v .: "c")
            <*> fmap (read @Float) (v .: "Q")
            <*> fmap (read @Float) (v .: "b")
            <*> fmap (read @Float) (v .: "B")
            <*> fmap (read @Float) (v .: "a")
            <*> fmap (read @Float) (v .: "A")
            <*> fmap (read @Float) (v .: "o")
            <*> fmap (read @Float) (v .: "h")
            <*> fmap (read @Float) (v .: "l")
            <*> fmap (read @Float) (v .: "v")
            <*> fmap (read @Float) (v .: "q")
            <*> v .: "O"
            <*> v .: "C"
            <*> v .: "F"
            <*> v .: "L"
            <*> v .: "N"

allMarketTickerOf :: TradingPair cName ->  StreamOf
     '[StreamType (AppendSymbol cName "@ticker") IndividualTickerResponse]
     '[IndividualTickerResponse]
allMarketTickerOf = streamOf @IndividualTickerResponse . IndividualTickerOf
