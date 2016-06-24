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
			h.subscribe(ws, msg.Payload["channel"])
		case "sendMessage":
			h.sendMessage(ws, msg.Payload["channel"], msg.Payload["message"])
		}
	}
}

func (h *pubsubHandler) subscribe(ws *websocket.Conn, channel string) {
	h.mutex.Lock()

	if conns, ok := h.channelsConns[channel]; ok {
		h.channelsConns[channel] = append(conns, ws)
	} else {
		h.channelsConns[channel] = []*websocket.Conn{ws}
	}

	if chans, ok := h.connsChannels[ws]; ok {
		h.connsChannels[ws] = append(chans, channel)
	} else {
		h.connsChannels[ws] = []string{channel}
	}

	h.mutex.Unlock()
}

func (h *pubsubHandler) sendMessage(ws *websocket.Conn, channel string, message string) {
	h.mutex.Lock()

	if conns, ok := h.channelsConns[channel]; ok {
		for _, c := range conns {
			if c != ws {
				if err := websocket.JSON.Send(c, &WsMsg{Type: "message", Payload: map[string]string{"channel": channel, "message": message}}); err != nil {
					log.Println("websocket.JSON.Send err:", err)
				}
			}
		}
	}

	h.mutex.Unlock()
}

func (h *pubsubHandler) cleanup(ws *websocket.Conn) {
	ws.Close()
}
