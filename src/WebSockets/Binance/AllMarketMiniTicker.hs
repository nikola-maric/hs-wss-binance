{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WebSockets.Binance.AllMarketMiniTicker where

import WebSockets.Binance.IndividualMiniTicker (IndividualMiniTickerResponse)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

newtype AllMarkerMiniTickerResponse = AllMarkerMiniTickerResponse {
        ammtrTickers :: [IndividualMiniTickerResponse]
    } 
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

