# hs-wss-binance

**IMPORTANT NOTE**: This is by no means production-grade or official library, it was built just for fun :) 

It is a client for Binance web socket streaming API, described [here](https://github.com/binance/binance-spot-api-docs/blob/master/web-socket-streams.md). However, only listening to stream messages is implemented, not the sending of messages to server.

Client uses [wuss](https://github.com/tfausak/wuss) library under the hood, which in turn uses [websockets](https://hackage.haskell.org/package/websockets) package.

Binance servers send PING messages from time to time, and if client does not respond in appropriate time, connection is closed. That part is being handled by `websockets` library automatically. But Binance will still close any connections that are open for more than 24h, that part needs to be handled by the user explicitly.

### Common Usage

`listenFor` is kinda the entrypoint, as it gets the stream and returns Chan that can be used to listen for messages.

There are some predefined streams in appropriate modules, like `tradeOf`, `allMarketBookTickerOf`, etc.

Streams can also be combined, so a single connection can be used to listen for multiple messages, using [world-peace](https://github.com/cdepillabout/world-peace) open sum implementation. For example, one can do

```haskell
main :: IO ()
main = do
  (priceInChan, priceOutChan) <- newChan 10
  concurrently_
    ( listenFor
        ( partialBookDepthOf (TradingPair @"adausdt") Five OneSecond
            `combineWith` differentialDepthOf (TradingPair @"bnbbtc") HundredMilliseconds
            `combineWith` tradeOf (TradingPair @"bnbbtc")
        )
        priceInChan
    )
    (readPrice priceOutChan)

readPrice :: Show a => OutChan a -> IO ()
readPrice channel = forever $ do
  msg <- readChan channel
  print msg
```

Resulting type that would be used in Chan-s would then look like

```haskell
(RecievePayload (OpenUnion '[PartialBookDepthStreamResponse, TradeResponse, DifferentialDepthStreamResponse]))
```

### Customizing responses

There is a little room for customization, if one does want to customize messages in any way - maybe we are not interested in all response fields, or there is some other reason.

If thats the case, one cam implement custom data type and define `FromJSON` and `ToJSON` instances, and create stream like so:

```haskell
data Blabla = Blabla {
    blablaQuantity :: Float,
    blablaBuyerOrderId :: Integer
} deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Blabla where
  parseJSON = withObject "Blabla" $ \v ->
    Blabla <$> fmap (read @Float) (v .: "q")
      <*> v .: "b"    

blablaTrade :: TradingPair cName -> StreamOf
     '[StreamType (AppendSymbol cName "@trade") Blabla] '[Blabla]
blablaTrade pair = streamOf @Blabla (TradingOf pair)
```
Listening to that stream would yield responses of type `Blabla` (again, wrapped in `PayloadResponse` and single element open sum)

