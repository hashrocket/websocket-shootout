{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}
module Main (main) where

import ClassyPrelude
import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as Ws


data Msg = Echo | Broadcast LByteString

parseMsg :: LByteString -> Maybe Msg
parseMsg msg = do
  Aeson.Object obj <- Aeson.decode msg
  Aeson.String typ <- lookup "type" obj

  case typ of
    "echo" -> Just Echo

    "broadcast" -> let
      res = Aeson.encode (insertMap "type" "broadcastResult" obj)
      in Just (Broadcast res)

    _ -> Nothing


-- Don't flood stdout after the benchmark
silentLoop :: IO () -> IO ()
silentLoop = handleAny (const $ pure ()) . forever

wsApp :: Unagi.InChan LByteString -> Ws.ServerApp
wsApp writeEnd pconn = do
  conn <- Ws.acceptRequest pconn
  readEnd <- Unagi.dupChan writeEnd
  void $ fork $ silentLoop (Unagi.readChan readEnd >>= Ws.sendTextData conn)

  silentLoop $ do
    msg <- Ws.receiveData conn

    case parseMsg msg of
      Nothing -> Ws.sendClose conn ("Invalid message" :: LByteString)

      Just Echo -> Ws.sendTextData conn msg

      Just (Broadcast res) -> do
        Unagi.writeChan writeEnd msg
        Ws.sendTextData conn res


backupApp :: Application
backupApp _ resp = resp $ responseLBS status400 [] "Not a WebSocket request"

main :: IO ()
main = do
  (writeEnd, _) <- Unagi.newChan
  run 3000 $ websocketsOr Ws.defaultConnectionOptions (wsApp writeEnd) backupApp
