{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module WebSockets.Binance.AllMarketMiniTicker where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WebSockets.Binance.IndividualMiniTicker (IndividualMiniTickerResponse)
import WebSockets.Binance.Stream (StreamOf, streamOf)
import WebSockets.Binance.Types (StreamType (AllMarketMiniTicker))

newtype AllMarkerMiniTickerResponse = AllMarkerMiniTickerResponse
  { ammtrTickers :: [IndividualMiniTickerResponse]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

allMarketMiniTickerOf ::
  StreamOf
    '[StreamType "!miniTicker@arr" AllMarkerMiniTickerResponse]
    '[AllMarkerMiniTickerResponse]
allMarketMiniTickerOf = streamOf @AllMarkerMiniTickerResponse AllMarketMiniTicker
