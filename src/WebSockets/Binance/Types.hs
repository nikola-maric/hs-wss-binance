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
    RecievePayload(..)
    , RawStreamResponse (..)
    , Currency(..)
    , ChartInterval (..)
    , DepthStreamLevel (..)
    , DepthStreamFrequency (..)
    , StreamType(..)
    , StreamOf(..)
    , SizeOf
    , JoinStreamNames
    , combineStreams
    , streamOf
    , streamName
    , streamPath
    -- ,reifySymbol, Curr(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, Value, withObject, (.:), Result (Error, Success), fromJSON)
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import Data.Kind (Type)
import Data.WorldPeace ( relaxOpenUnion, Contains, OpenUnion )
import GHC.TypeLits (KnownNat, type (+), Symbol, AppendSymbol, KnownSymbol, Nat, symbolVal, natVal, SomeSymbol(..), someSymbolVal)
import Data.Data (Proxy)
import Data.Typeable (Proxy(Proxy))
import Unsafe.Coerce (unsafeCoerce)

newtype StreamOf (s :: [Type]) (a :: [Type]) = StreamOf {
    parseMessage :: RawStreamResponse -> RecievePayload (OpenUnion a)
}

streamName :: (KnownSymbol (JoinStreamNames s)) => StreamOf s a -> String
streamName (_ :: StreamOf s a) = symbolVal (Proxy :: Proxy (JoinStreamNames s))

streamPath :: (KnownNat (SizeOf s)) => StreamOf s a -> String
streamPath s = if streamSourcesSize s > 1 then "/stream?streams=" else "/ws/"

data CombinedStreamResponse = CombinedStreamResponse {
    csrStreamName :: String,
    csrData :: Value
  }   deriving stock (Eq, Show, Generic)
      deriving anyclass ToJSON

data RawStreamResponse = FromCombined CombinedStreamResponse | RawMessage WS.DataMessage

instance FromJSON CombinedStreamResponse where
  parseJSON = withObject "CombinedStreamResponse" $ \v ->
        CombinedStreamResponse <$> v .: "stream"
            <*> v .: "data"

data RecievePayload a = Successfull a | Failed String
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance (ToJSON a, FromJSON a) => WS.WebSocketsData (RecievePayload a) where
    fromDataMessage (WS.Text   bl _) = WS.fromLazyByteString bl
    fromDataMessage (WS.Binary bl)   = WS.fromLazyByteString bl
    fromLazyByteString lbs = case Aeson.eitherDecode lbs of
        Right val -> Successfull val
        Left msg  -> Failed msg
    toLazyByteString (Successfull val)  = Aeson.encode val -- Why do we need this if we are only receiving?
    toLazyByteString (Failed val)       = Aeson.encode val -- Why do we need this if we are only receiving?

-- We can avoid type applications on call sites, but that way it would get hardcoded :
-- nobody could define a stream of something that returns some other result, apart from, say, TradeResponse
data StreamType (currencySymbol :: Symbol) r where
    AggregatedTradeOf :: Currency cName -> StreamType (AppendSymbol cName "@aggTrade") r 
    TradingOf :: Currency cName -> StreamType (AppendSymbol cName "@trade" ) r
    CandlestickDataOf :: Currency cName -> ChartInterval tName -> StreamType (AppendSymbol (AppendSymbol (AppendSymbol  cName "@") "kline_") tName) r
    IndividualMiniTickerOf :: Currency cName -> StreamType (AppendSymbol cName "@miniTicker") r
    IndividualTickerOf :: Currency cName -> StreamType (AppendSymbol cName "@ticker") r
    AllMarketMiniTickerOf :: StreamType "!miniTicker@arr" r
    AllMarketTickerOf :: StreamType "!ticker@arr" r
    IndividualBookTickerOf :: Currency cName -> StreamType (AppendSymbol cName "@bookTicker") r
    AllMarketBookTickerOf :: StreamType "!bookTicker" r
    PartialBookDepthStreamOf :: Currency cName -> DepthStreamLevel depth -> DepthStreamFrequency freq -> 
        StreamType (AppendSymbol freq (AppendSymbol "@" (AppendSymbol depth (AppendSymbol cName "@depth")))) r
    DiffDepthStreamOf :: Currency cName -> DepthStreamFrequency freq ->
        StreamType (AppendSymbol freq (AppendSymbol cName "@depth@")) r

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

data Currency (name :: Symbol) where
    ADAUSDT :: Currency "adausdt"
    BNBBTC :: Currency "bnbbtc"

streamOf :: (FromJSON r, ToJSON r) => StreamType c r -> StreamOf '[StreamType c r] '[r]
streamOf (_ :: StreamType c r) = StreamOf { parseMessage = parseSingle }

combineStreams :: (as ~ Flatten '[a, a'], Contains a as, Contains a' as) => StreamOf s a -> StreamOf s' a' -> StreamOf (Merge s s') as
combineStreams s1 s2 = StreamOf {
  parseMessage = \v ->
    case parseCombined v of -- we know this is a combined stream : if packing it to proper response fails, fail everything
      Failed reason -> Failed reason
      Successfull packed -> case parseMessage s1 packed of -- at this point we no longer work with RawMessage
        Failed _ -> case parseMessage s2 packed of
          Failed reason -> Failed reason
          Successfull y -> Successfull (relaxOpenUnion y)
        Successfull x -> Successfull (relaxOpenUnion x)
}

parseSingle :: forall r. (FromJSON r, ToJSON r) => RawStreamResponse -> RecievePayload r
parseSingle (RawMessage msg) = WS.fromDataMessage @(RecievePayload r) msg
parseSingle (FromCombined csr) = case fromJSON @r (csrData csr) of
    Error reason -> Failed reason
    Success x -> Successfull x

parseCombined :: RawStreamResponse -> RecievePayload RawStreamResponse
parseCombined (RawMessage msg) = case WS.fromDataMessage @(RecievePayload CombinedStreamResponse) msg of
  Failed reason -> Failed reason
  Successfull packed -> Successfull (FromCombined packed)
parseCombined (FromCombined msg) = Successfull (FromCombined msg)

streamSourcesSize :: (KnownNat (SizeOf s)) => StreamOf s a -> Integer
streamSourcesSize (_ :: StreamOf s a) = natVal (Proxy :: Proxy (SizeOf s))

type family Merge (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Merge '[] ys = ys
  Merge xs '[] = xs
  Merge(x ': xs) (y ': ys) = x ': y ': Merge xs ys

type family Flatten (xss :: [[Type]]) :: [Type] where
  Flatten '[] = '[]
  Flatten (as ': ass) = Merge as (Flatten ass) -- UndecidableInstances?

type family SizeOf xs :: Nat where
  SizeOf '[] = 0
  SizeOf (x ': xs) = 1 + SizeOf xs

-- TODO : this only works if all types are StreamPath. If last element is not, we will have extra '/'. Better way?
type family JoinStreamNames (x :: [Type]) :: Symbol where
  JoinStreamNames '[] = ""
  JoinStreamNames (StreamType sName result ': '[]) = sName
  JoinStreamNames (StreamType sName result ': xs) = AppendSymbol (AppendSymbol sName "/") (JoinStreamNames xs)
  JoinStreamNames (x ': xs) = JoinStreamNames xs


-- newtype MagicSymbol r = MagicSymbol (forall (n :: Symbol). KnownSymbol n => Proxy n -> r)

-- data Curr (s :: Symbol) = Curre

-- instance KnownSymbol s => Show (Curr s) where
--     show (_ :: Curr s) = symbolVal (Proxy :: Proxy s)

-- reifySymbol :: forall r. String -> (forall (n :: Symbol). KnownSymbol n => Proxy n -> r) -> r
-- reifySymbol n k = unsafeCoerce (MagicSymbol k :: MagicSymbol r) n Proxy

-- createCurrency str = reifySymbol str (\(_ :: Proxy abc) -> Curre)