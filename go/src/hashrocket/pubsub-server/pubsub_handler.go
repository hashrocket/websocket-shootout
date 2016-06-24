package main

import (
	"io"
	"log"
	"sync"

	"golang.org/x/net/websocket"
)

type pubsubHandler struct {
	mutex         sync.Mutex
	channelsConns map[string][]*websocket.Conn
	connsChannels map[*websocket.Conn][]string
}

type WsMsg struct {
	Type    string            `json:"Type"`
	Payload map[string]string `json:"payload"`
}

func NewPubsubHandler() *pubsubHandler {
	return &pubsubHandler{
		channelsConns: make(map[string][]*websocket.Conn),
		connsChannels: make(map[*websocket.Conn][]string),
	}
}

func (h *pubsubHandler) Accept(ws *websocket.Conn) {
	defer h.cleanup(ws)

	for {
		var msg WsMsg
		if err := websocket.JSON.Receive(ws, &msg); err == io.EOF {
			return
		} else if err != nil {
			log.Println("websocket.JSON.Receive err:", err)
			return
		}

		switch msg.Type {
		case "subscribe":
			log.Println("subscribe")
		case "sendMessage":
			log.Println("sendMessage")
		}
	}
}

func (h *pubsubHandler) subscribe(ws *websocket.Conn, channel string) {
}

func (h *pubsubHandler) cleanup(ws *websocket.Conn) {
	ws.Close()
}
