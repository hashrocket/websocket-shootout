{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}
module Main (main) where

import ClassyPrelude
import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as Ws

-- Normally common parts would be extracted into a library, but
-- it's a demonstration so I'll duplicate them for ease of reference

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


wsApp :: IORef (HashMap ThreadId Ws.Connection) -> Ws.ServerApp
wsApp clients pconn = do
  conn <- Ws.acceptRequest pconn
  self <- myThreadId
  atomicModifyIORef clients $ \m -> (insertMap self conn m, ())
  let removeSelf = atomicModifyIORef clients $ \m -> (deleteMap self m, ())

  handleAny (const removeSelf) $ forever $ do
    msg <- Ws.receiveData conn

    case parseMsg msg of
      Nothing -> Ws.sendClose conn ("Invalid message" :: LByteString)

      Just Echo -> Ws.sendTextData conn msg

      Just (Broadcast res) -> do
        cs <- readIORef clients
        mapM_ (flip Ws.sendTextData msg) (toList cs)
        Ws.sendTextData conn res


backupApp :: Application
backupApp _ resp = resp $ responseLBS status400 [] "Not a WebSocket request"

main :: IO ()
main = do
  clients <- newIORef (mapFromList [])
  run 3000 $ websocketsOr Ws.defaultConnectionOptions (wsApp clients) backupApp
