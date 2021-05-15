{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE DataKinds #-}
module WebSockets.Binance.IndividualMiniTicker where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)
import WebSockets.Binance.Types
    ( TradingPair, StreamType(IndividualMiniTickerOf) )
import WebSockets.Binance.Stream ( StreamOf, streamOf )
import GHC.TypeLits (AppendSymbol)
import Data.Text (Text)

data IndividualMiniTickerResponse = IndividualMiniTickerResponse
  { imtrEventType :: Text,
    imtrEventTime :: Integer,
    imtrEventSymbol :: Text,
    imtrClosePrice :: Float,
    imtrOpenPrice :: Float,
    imtrHighPrice :: Float,
    imtrLowPrice :: Float,
    imtrTotalTradedBaseAssetVolume :: Float,
    imtrTotalTradedQuoteAssetVolume :: Float
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON IndividualMiniTickerResponse where
  parseJSON = withObject "IndividualMiniTickerResponse" $ \v ->
    IndividualMiniTickerResponse <$> v .: "e"
      <*> v .: "E"
      <*> v .: "s"
      <*> fmap (read @Float) (v .: "c")
      <*> fmap (read @Float) (v .: "o")
      <*> fmap (read @Float) (v .: "h")
      <*> fmap (read @Float) (v .: "l")
      <*> fmap (read @Float) (v .: "v")
      <*> fmap (read @Float) (v .: "q")

individualMiniTickerOf :: TradingPair cName -> StreamOf
     '[StreamType (AppendSymbol cName "@miniTicker") IndividualMiniTickerResponse]
     '[IndividualMiniTickerResponse]
individualMiniTickerOf = streamOf @IndividualMiniTickerResponse . IndividualMiniTickerOf