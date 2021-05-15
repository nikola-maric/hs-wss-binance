{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebSockets.Binance.AllMarketMiniTicker where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WebSockets.Binance.IndividualMiniTicker (IndividualMiniTickerResponse)

newtype AllMarkerMiniTickerResponse = AllMarkerMiniTickerResponse
  { ammtrTickers :: [IndividualMiniTickerResponse]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)
