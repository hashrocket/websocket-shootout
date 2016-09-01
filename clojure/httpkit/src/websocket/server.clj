(ns websocket.server
  (:require [compojure.core :refer [GET defroutes]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [clojure.tools.logging :as log]
            [cheshire.core :as json]
            [org.httpkit.server :refer [send! with-channel on-close on-receive]]))

(defonce channels (atom #{}))

(defn connect! [channel]
  (log/info "channel open")
  (swap! channels conj channel))

(defn disconnect! [channel status]
  (log/info "channel closed:" status)
  (swap! channels disj channel))

(defn broadcast [ch payload]
  (let [msg (json/encode {:type "broadcast" :payload payload})]
    (doseq [channel @channels]
      (send! channel msg)))
  (send! ch (json/encode {:type "broadcastResult" :payload payload})))

(defn echo [ch payload]
  (send! ch (json/encode {:type "echo" :payload payload})))

(defn unknown-type-response [ch _]
  (send! ch (json/encode {:type "error" :payload "ERROR: unknown message type"})))

(defn dispatch [ch msg]
  (let [parsed (json/decode msg)]
    ((case (get parsed "type")
        "echo" echo
        "broadcast" broadcast
        unknown-type-response)
      ch (get parsed "payload"))))

(defn ws-handler [request]
  (with-channel request channel
    (connect! channel)
    (on-close channel (partial disconnect! channel))
    (on-receive channel #(dispatch channel %))))

(defroutes websocket-routes
  (GET "/ws" request (ws-handler request)))

(def app
  (wrap-defaults websocket-routes api-defaults))

