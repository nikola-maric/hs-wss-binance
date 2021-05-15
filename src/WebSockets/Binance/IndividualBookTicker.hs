{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WebSockets.Binance.IndividualBookTicker where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)
import GHC.TypeLits (AppendSymbol)
import WebSockets.Binance.Types
    ( TradingPair,
      StreamType(AllMarketBookTicker, IndividualBookTickerOf) )
import WebSockets.Binance.Stream ( StreamOf, streamOf )

data IndividualBookTickerResponse = IndividualBookTickerResponse
  { ibtrOrderBookUpdateId :: Integer,
    ibtrSymbol :: String,
    ibtrBestBidPrice :: Float,
    ibtrBestBidQuantity :: Float,
    ibtrBestAskPrice :: Float,
    ibtrBestAskQuantity :: Float
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON IndividualBookTickerResponse where
  parseJSON = withObject "IndividualBookTickerResponse" $ \v ->
    IndividualBookTickerResponse <$> v .: "u"
      <*> v .: "s"
      <*> fmap (read @Float) (v .: "b")
      <*> fmap (read @Float) (v .: "B")
      <*> fmap (read @Float) (v .: "a")
      <*> fmap (read @Float) (v .: "A")

individualBookTickerStreamOf ::
  TradingPair pair ->
  StreamOf
    '[StreamType (AppendSymbol pair "@bookTicker") IndividualBookTickerResponse]
    '[IndividualBookTickerResponse]
individualBookTickerStreamOf pair = streamOf @IndividualBookTickerResponse (IndividualBookTickerOf pair)

allMarketBookTickerStream ::
  StreamOf
    '[StreamType "!bookTicker" IndividualBookTickerResponse]
    '[IndividualBookTickerResponse]
allMarketBookTickerStream = streamOf @IndividualBookTickerResponse AllMarketBookTicker
