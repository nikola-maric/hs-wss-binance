{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module WebSockets.Binance.Types (
    TradingPair(..)
    , ChartInterval (..)
    , DepthStreamLevel (..)
    , DepthStreamFrequency (..)
    , StreamType(..)
    ) where

import GHC.TypeLits (Symbol, AppendSymbol)

data StreamType (currencySymbol :: Symbol) r where
    AggregatedTradeOf :: TradingPair cName -> StreamType (AppendSymbol cName "@aggTrade") r 
    TradingOf :: TradingPair cName -> StreamType (AppendSymbol cName "@trade" ) r
    CandlestickDataOf :: TradingPair cName -> ChartInterval tName -> StreamType (AppendSymbol (AppendSymbol (AppendSymbol  cName "@") "kline_") tName) r
    IndividualMiniTickerOf :: TradingPair cName -> StreamType (AppendSymbol cName "@miniTicker") r
    IndividualTickerOf :: TradingPair cName -> StreamType (AppendSymbol cName "@ticker") r
    AllMarketMiniTicker :: StreamType "!miniTicker@arr" r
    AllMarketTicker :: StreamType "!ticker@arr" r
    IndividualBookTickerOf :: TradingPair cName -> StreamType (AppendSymbol cName "@bookTicker") r
    AllMarketBookTicker :: StreamType "!bookTicker" r
    PartialBookDepthStreamOf :: TradingPair cName -> DepthStreamLevel depth -> DepthStreamFrequency freq -> 
        StreamType (AppendSymbol cName (AppendSymbol "@depth" (AppendSymbol depth (AppendSymbol "@" freq)))) r
    DiffDepthStreamOf :: TradingPair cName -> DepthStreamFrequency freq ->
        StreamType (AppendSymbol cName (AppendSymbol "@depth@" freq)) r

data ChartInterval (name :: Symbol) where
    OneMinute :: ChartInterval "1m"
    ThreeMinutes :: ChartInterval "2m"
    FiveMinutes :: ChartInterval "5m"
    FifteenMinutes :: ChartInterval "15m"
    ThirtyMinutes :: ChartInterval "30m" 
    OneHour :: ChartInterval "1h"
    TwoHours :: ChartInterval "2h"
    FourHours :: ChartInterval "4h"
    SixHours :: ChartInterval "6h"
    EightHours :: ChartInterval "8h"
    TweleveHours :: ChartInterval "12h"
    OneDay :: ChartInterval "1d"
    ThreeDays :: ChartInterval "3d"
    OneWeek :: ChartInterval "1w"
    OneMonth :: ChartInterval "1M"

data DepthStreamLevel (name :: Symbol) where 
    Five :: DepthStreamLevel "5"
    Ten :: DepthStreamLevel "10"
    Twenty :: DepthStreamLevel "20"

data DepthStreamFrequency (name :: Symbol) where
    HundredMilliseconds :: DepthStreamFrequency "100ms"
    OneSecond :: DepthStreamFrequency "1000ms"

data TradingPair (name :: Symbol) = TradingPair