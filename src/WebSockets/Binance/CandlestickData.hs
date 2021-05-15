{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WebSockets.Binance.CandlestickData where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)
import GHC.TypeLits (AppendSymbol)
import WebSockets.Binance.Stream (StreamOf, streamOf)
import WebSockets.Binance.Types (ChartInterval, StreamType (CandlestickDataOf), TradingPair)
import Data.Text (Text)

data CandlestickDataResponse = CandlestickDataResponse
  { cdrEventType :: Text,
    cdrEventTime :: Integer,
    cdrSymbol :: Text,
    cdrKlineStartTime :: Integer,
    cdrKlineCloseTime :: Integer,
    cdrKlineSymbol :: Text,
    cdrKlineInteval :: Text,
    cdrKlineFirstTradeId :: Integer,
    cdrKlineLastTradeId :: Integer,
    cdrKlineOpenPrice :: Float,
    cdrKlineClosePrice :: Float,
    cdrKlineHighPrice :: Float,
    cdrKlineLowPrice :: Float,
    cdrKlineBaseAssetVolume :: Float,
    cdrKlineNumberOfTrades :: Integer,
    cdrKlineClosed :: Bool,
    cdrKlineQuoteAssetVolume :: Float,
    cdrKlineTakerBuyBaseAssetVolume :: Float,
    cdrKlineTakerBuyQuoteAssetVolume :: Float
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON CandlestickDataResponse where
  parseJSON = withObject "CandlestickData" $ \v ->
    CandlestickDataResponse <$> v .: "e"
      <*> v .: "E"
      <*> v .: "s"
      <*> ((v .: "k") >>= (.: "t"))
      <*> ((v .: "k") >>= (.: "T"))
      <*> ((v .: "k") >>= (.: "s"))
      <*> ((v .: "k") >>= (.: "i"))
      <*> ((v .: "k") >>= (.: "f"))
      <*> ((v .: "k") >>= (.: "L"))
      <*> fmap (read @Float) ((v .: "k") >>= (.: "o"))
      <*> fmap (read @Float) ((v .: "k") >>= (.: "c"))
      <*> fmap (read @Float) ((v .: "k") >>= (.: "h"))
      <*> fmap (read @Float) ((v .: "k") >>= (.: "l"))
      <*> fmap (read @Float) ((v .: "k") >>= (.: "v"))
      <*> ((v .: "k") >>= (.: "n"))
      <*> ((v .: "k") >>= (.: "x"))
      <*> fmap (read @Float) ((v .: "k") >>= (.: "q"))
      <*> fmap (read @Float) ((v .: "k") >>= (.: "V"))
      <*> fmap (read @Float) ((v .: "k") >>= (.: "Q"))

candlestickDataOf ::
  TradingPair cName ->
  ChartInterval tName ->
  StreamOf
    '[StreamType (AppendSymbol (AppendSymbol (AppendSymbol cName "@") "kline_") tName) CandlestickDataResponse]
    '[CandlestickDataResponse]
candlestickDataOf pair interval = streamOf @CandlestickDataResponse (CandlestickDataOf pair interval)
