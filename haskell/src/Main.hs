{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Control.Concurrent as C
import Control.Concurrent.Chan.Unagi
import Control.Lens hiding ((.=))
import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.Char as DC
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Network.HTTP.Types (status400)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Text.RawString.QQ

amendTest :: Maybe Value
amendTest = decode $ [r|
{"type":"broadcast","payload":{"foo": "bar"}}
|]

amendBroadcast :: Value -> Value
amendBroadcast v =
  v & key "type" . _String .~ "broadcastResult"

broadcastThread :: OutChan ByteString -> Connection -> IO ()
broadcastThread bc conn = forever $ do
  t <- readChan bc
  sendTextData conn t

wtf conn =
  sendTextData conn ("<img src=\"http://bit.ly/1kmRC7Q\" />" :: Text)

mkPayload :: Text -> Value -> ByteString
mkPayload type_ payload = encode $
  object [ "type" .= String type_
         , "payload" .= payload
         ]

bidiHandler :: InChan ByteString -> Connection -> IO ()
bidiHandler inp conn = do
  outp <- dupChan inp
  _ <- C.forkIO (broadcastThread outp conn)
  forever $ do
    msg <- receiveDataMessage conn
    case msg of
      Text t -> do
        let Just payload = t ^? key "payload"
        case t ^? key "type" . _String of
          Just "echo" -> sendTextData conn (mkPayload "echo" payload)
          Just "broadcast" -> writeChan inp (mkPayload "broadcastResult" payload)
          _ -> wtf conn
      _ -> do
        wtf conn

wsApp :: InChan ByteString -> ServerApp
wsApp inp pending = do
  conn <- acceptRequest pending
  bidiHandler inp conn

main :: IO ()
main = do
  (inp, outp) <- newChan
  -- This is really unfortunate... We need some lazy IO like
  -- IO (InChan, [OutChan]) from where we can draw duplicate output channels
  -- transparently. Otherwise the prototypical OutChan will just accumulate
  -- payloads until it overflows
  C.forkIO $ forever $ readChan outp
  runServer "127.0.0.1" 3000 (wsApp inp)
