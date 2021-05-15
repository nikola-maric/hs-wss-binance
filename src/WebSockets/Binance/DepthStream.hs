{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module WebSockets.Binance.DepthStream where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withArray, withObject, (.:))
import Data.Aeson.Types (Value (..), typeMismatch)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.TypeLits (AppendSymbol)
import WebSockets.Binance.Types
    ( TradingPair,
      DepthStreamFrequency,
      DepthStreamLevel,
      StreamType(DiffDepthStreamOf, PartialBookDepthStreamOf) )
import WebSockets.Binance.Stream ( StreamOf, streamOf )

data Bid = Bid
  { bidPriceLevel :: Float,
    bidQuantity :: Float
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Bid where
  parseJSON = withArray "Bid" $ \arr ->
    if V.length arr /= 2
      then typeMismatch "Bid array length should be 2" (Array arr)
      else Bid <$> fmap (read @Float) (parseJSON (arr V.! 0)) <*> fmap (read @Float) (parseJSON (arr V.! 1))

data Ask = Ask
  { askPriceLevel :: Float,
    askQuantity :: Float
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Ask where
  parseJSON = withArray "Ask" $ \arr ->
    if V.length arr /= 2
      then typeMismatch "Ask array length should be 2" (Array arr)
      else Ask <$> fmap (read @Float) (parseJSON (arr V.! 0)) <*> fmap (read @Float) (parseJSON (arr V.! 1))

data PartialBookDepthStreamResponse = PartialBookDepthStreamResponse
  { pbdsrLastUpdateId :: Integer,
    pbdsrBids :: [Bid],
    pbdsrAsks :: [Ask]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON PartialBookDepthStreamResponse where
  parseJSON = withObject "PartialBookDepthStreamResponse" $ \v ->
    PartialBookDepthStreamResponse <$> v .: "lastUpdateId" <*> v .: "bids" <*> v .: "asks"

data DifferentialDepthStreamResponse = DifferentialDepthStreamResponse
  { ddsrEventType :: String,
    ddsrEventTime :: Integer,
    ddsrSymbol :: String,
    ddsrFirstUpdateId :: Integer,
    ddsrFinalUpdateId :: Integer,
    ddsrBids :: [Bid],
    ddsrAsks :: [Ask]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON DifferentialDepthStreamResponse where
  parseJSON = withObject "DifferentialDepthStreamResponse" $ \v ->
    DifferentialDepthStreamResponse
      <$> v .: "e"
      <*> v .: "E"
      <*> v .: "s"
      <*> v .: "U"
      <*> v .: "u"
      <*> v .: "b"
      <*> v .: "a"

partialBookDepthStreamOf ::
  TradingPair cName ->
  DepthStreamLevel depth ->
  DepthStreamFrequency freq ->
  StreamOf
    '[StreamType (AppendSymbol cName (AppendSymbol "@depth" (AppendSymbol depth (AppendSymbol "@" freq)))) PartialBookDepthStreamResponse]
    '[PartialBookDepthStreamResponse]
partialBookDepthStreamOf pair depth frequency = streamOf @PartialBookDepthStreamResponse (PartialBookDepthStreamOf pair depth frequency)

differentialDepthStreamOf ::
  TradingPair cName ->
  DepthStreamFrequency freq ->
  StreamOf
    '[StreamType (AppendSymbol cName (AppendSymbol "@depth@" freq)) DifferentialDepthStreamResponse]
    '[DifferentialDepthStreamResponse]
differentialDepthStreamOf pair frequency = streamOf @DifferentialDepthStreamResponse (DiffDepthStreamOf pair frequency)
