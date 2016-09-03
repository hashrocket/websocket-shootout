{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Broadcast as BC
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

type Broadcaster = BC.Broadcast ByteString

amendTest :: Maybe Value
amendTest = decode $ [r|
{"type":"broadcast","payload":{"foo": "bar"}}
|]

amendBroadcast :: Value -> Value
amendBroadcast v =
  v & key "type" . _String .~ "broadcastResult"

broadcastThread :: Broadcaster -> Connection -> IO ()
broadcastThread bc conn = forever $ do
  t <- BC.listen bc
  sendTextData conn t

wtf conn =
  sendTextData conn ("<img src=\"http://bit.ly/1kmRC7Q\" />" :: Text)

mkPayload :: Text -> Value -> ByteString
mkPayload type_ payload = encode $
  object [ "type" .= String type_
         , "payload" .= payload
         ]

bidiHandler :: Broadcaster -> Connection -> IO ()
bidiHandler bc conn = do
  _ <- C.forkIO (broadcastThread bc conn)
  forever $ do
    msg <- receiveDataMessage conn
    case msg of
      Text t -> do
        let Just payload = t ^? key "payload"
        case t ^? key "type" . _String of
          Just "echo" -> sendTextData conn (mkPayload "echo" payload)
          Just "broadcast" -> BC.signal bc (mkPayload "broadcastResult" payload)
          _ -> wtf conn
      _ -> do
        wtf conn

wsApp :: Broadcaster -> ServerApp
wsApp bc pending = do
  conn <- acceptRequest pending
  bidiHandler bc conn

main :: IO ()
main = do
  bc <- BC.new
  runServer "127.0.0.1" 3000 (wsApp bc)
