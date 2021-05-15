{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebSockets.Binance.AllMarketTicker where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WebSockets.Binance.IndividualMiniTicker (IndividualMiniTickerResponse)

newtype AllMarketTickerResponse = AllMarketTickerResponse
  { amtrTickers :: [IndividualMiniTickerResponse]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)
