
{-# LANGUAGE OverloadedStrings #-}

-- haskell-transient application for the websocket shootout
-- https://github.com/hashrocket/websocket-shootout

-- this is the implementation of the benchmark with the transient libraries
-- https://github.com/transient-haskell

import Transient.Internals
import Transient.Move.Internals
import Transient.EVars
import Transient.Logged
import Transient.Move.Utils
import Data.Aeson as Aeson
import Data.Containers as H
import Control.Applicative

main= keep' $ do
  broad <- newEVar                  -- An EVar is a channel with de-inverted logic and nondeterminist, see below
  initNodeDef "localhost" 3000 $ apisample broad 
  return ()

apisample broad= api $               -- api executes the rest of the computation when some message arrives.
                                     -- It call the computation with an empty message when there is a new
                                     -- connection, so watchbroadcast may be called
                                     -- messages are passed to the computation trough a state monad
                                     -- each response of the computation, either single message or broadcast are
                                     -- forwarded by "api" to the sender trough the websocket.
                                     
    do   msg <- param                -- param read the message in the state. if the message is empty, it fails and
                                     -- watchBroadcast- is executed.
         processMessage broad msg
         
    <|>  watchBroadcast broad         -- watch for broadcast messages 
                                      -- Since it is called once at the beginning of each socket connection
                                      -- it will be as much readEVar watching as the number of connections



processMessage broad msg= do
    Aeson.Object obj <- emptyIfNothing $ Aeson.decode msg
    Aeson.String typ <- emptyIfNothing $ H.lookup "type" obj
    case typ of
       "echo" -> return msg
       "broadcast" ->  do
                  let res = Aeson.encode  $ insertMap "type" "broadcastResult" obj
                  writeEVar broad msg                                -- write in the broadcast channel
                  return res


-- The EVar read the broadcast channel and return the results. It would generate a thread for
-- each response, but since `threads 0` is present, it uses a single thread in a loop.
-- Since it is called once at the beginning of each socket connection
-- it will be as much readEVar watching as the number of connections

watchBroadcast broad= threads 0 $ readEVar broad

