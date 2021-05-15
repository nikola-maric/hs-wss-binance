{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- without as ~ Flatten '[a, a'] constraint on `combineWith` we get OpenUnion errors
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module WebSockets.Binance.Stream
  ( RecievePayload (..),
    RawStreamResponse (..),
    StreamOf (..),
    SizeOf,
    JoinStreamNames,
    combineWith,
    streamOf,
    streamName,
    streamPath,
  )
where

import Data.Aeson (FromJSON, Result (Error, Success), ToJSON, Value, fromJSON, withObject, (.:))
import qualified Data.Aeson as Aeson
import Data.Data (Proxy)
import Data.Kind (Type)
import Data.Typeable (Proxy (Proxy))
import Data.WorldPeace (Contains, OpenUnion, relaxOpenUnion)
import GHC.Generics (Generic)
import GHC.TypeLits (AppendSymbol, KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal, type (+))
import qualified Network.WebSockets as WS
import WebSockets.Binance.Types (StreamType)

newtype StreamOf (s :: [Type]) (a :: [Type]) = StreamOf
  { parseMessage :: RawStreamResponse -> RecievePayload (OpenUnion a)
  }

streamName :: (KnownSymbol (JoinStreamNames s)) => StreamOf s a -> String
streamName (_ :: StreamOf s a) = symbolVal (Proxy :: Proxy (JoinStreamNames s))

streamPath :: (KnownNat (SizeOf s)) => StreamOf s a -> String
streamPath s = if streamSourcesSize s > 1 then "/stream?streams=" else "/ws/"

data CombinedStreamResponse = CombinedStreamResponse {csrStreamName :: String, csrData :: Value}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON CombinedStreamResponse where
  parseJSON = withObject "CombinedStreamResponse" $ \v ->
    CombinedStreamResponse <$> v .: "stream"
      <*> v .: "data"

data RawStreamResponse = FromCombined CombinedStreamResponse | RawMessage WS.DataMessage

data RecievePayload a = Successfull a | Failed String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance (ToJSON a, FromJSON a) => WS.WebSocketsData (RecievePayload a) where
  fromDataMessage (WS.Text bl _) = WS.fromLazyByteString bl
  fromDataMessage (WS.Binary bl) = WS.fromLazyByteString bl
  fromLazyByteString lbs = case Aeson.eitherDecode lbs of
    Right val -> Successfull val
    Left msg -> Failed msg
  toLazyByteString (Successfull val) = Aeson.encode val -- Why do we need this if we are only receiving?
  toLazyByteString (Failed val) = Aeson.encode val -- Why do we need this if we are only receiving?

streamOf :: (FromJSON r, ToJSON r) => StreamType c r -> StreamOf '[StreamType c r] '[r]
streamOf (_ :: StreamType c r) = StreamOf {parseMessage = parseSingle}

combineWith :: (as ~ Flatten '[a, a'], Contains a as, Contains a' as) => StreamOf s a -> StreamOf s' a' -> StreamOf (Merge s s') as
combineWith s1 s2 =
  StreamOf
    { parseMessage = \v ->
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
  Merge (x ': xs) (y ': ys) = x ': y ': Merge xs ys

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
