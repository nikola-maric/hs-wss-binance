{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WebSockets.Binance.AllMarketTicker where

import WebSockets.Binance.IndividualMiniTicker (IndividualMiniTickerResponse)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

newtype AllMarketTickerResponse = AllMarketTickerResponse {
        amtrTickers :: [IndividualMiniTickerResponse]
    }
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON)