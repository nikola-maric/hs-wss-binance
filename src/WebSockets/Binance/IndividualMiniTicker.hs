{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module WebSockets.Binance.IndividualMiniTicker where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import GHC.Generics (Generic)

data IndividualMiniTickerResponse = IndividualMiniTickerResponse {
        imtrEventType                   :: String,
        imtrEventTime                   :: Integer,
        imtrEventSymbol                 :: String,
        imtrClosePrice                  :: Float,
        imtrOpenPrice                   :: Float,
        imtrHighPrice                   :: Float,
        imtrLowPrice                    :: Float,
        imtrTotalTradedBaseAssetVolume  :: Float,
        imtrTotalTradedQuoteAssetVolume :: Float
    } deriving stock (Eq, Show, Generic)
      deriving anyclass ToJSON

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