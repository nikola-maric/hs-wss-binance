{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module WebSockets.Binance.AllMarketTicker where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WebSockets.Binance.IndividualTicker (IndividualTickerResponse)
import WebSockets.Binance.Stream (StreamOf, streamOf)
import WebSockets.Binance.Types (StreamType (AllMarketTicker))

newtype AllMarketTickerResponse = AllMarketTickerResponse
  { amtrTickers :: [IndividualTickerResponse]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

allMarketTickerOf ::
  StreamOf
    '[StreamType "!ticker@arr" AllMarketTickerResponse]
    '[AllMarketTickerResponse]
allMarketTickerOf = streamOf @AllMarketTickerResponse AllMarketTicker
