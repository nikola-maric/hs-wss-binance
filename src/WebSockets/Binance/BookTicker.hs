{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WebSockets.Binance.BookTicker where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)
import GHC.TypeLits (AppendSymbol)
import WebSockets.Binance.Stream (StreamOf, streamOf)
import WebSockets.Binance.Types
  ( StreamType (AllMarketBookTicker, IndividualBookTickerOf),
    TradingPair,
  )
import Data.Text (Text)

data BookTickerResponse = IndividualBookTickerResponse
  { btrOrderBookUpdateId :: Integer,
    btrSymbol :: Text,
    btrBestBidPrice :: Float,
    btrBestBidQuantity :: Float,
    btrBestAskPrice :: Float,
    btrBestAskQuantity :: Float
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON BookTickerResponse where
  parseJSON = withObject "IndividualBookTickerResponse" $ \v ->
    IndividualBookTickerResponse <$> v .: "u"
      <*> v .: "s"
      <*> fmap (read @Float) (v .: "b")
      <*> fmap (read @Float) (v .: "B")
      <*> fmap (read @Float) (v .: "a")
      <*> fmap (read @Float) (v .: "A")

individualBookTickerOf ::
  TradingPair pair ->
  StreamOf
    '[StreamType (AppendSymbol pair "@bookTicker") BookTickerResponse]
    '[BookTickerResponse]
individualBookTickerOf pair = streamOf @BookTickerResponse (IndividualBookTickerOf pair)

allMarketBookTickerOf ::
  StreamOf
    '[StreamType "!bookTicker" BookTickerResponse]
    '[BookTickerResponse]
allMarketBookTickerOf = streamOf @BookTickerResponse AllMarketBookTicker
