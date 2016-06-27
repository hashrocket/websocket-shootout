package main

import (
	"io"
	"log"
	"sync"

	"golang.org/x/net/websocket"
)

type pubsubHandler struct {
	mutex         sync.Mutex
	channelsConns map[string]map[*websocket.Conn]struct{}
	connsChannels map[*websocket.Conn][]string
}

type WsMsg struct {
	Type    string            `json:"Type"`
	Payload map[string]string `json:"payload"`
}

func NewPubsubHandler() *pubsubHandler {
	return &pubsubHandler{
		channelsConns: make(map[string]map[*websocket.Conn]struct{}),
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
		conns[ws] = struct{}{}
	} else {
		conns = make(map[*websocket.Conn]struct{})
		conns[ws] = struct{}{}
		h.channelsConns[channel] = conns
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
		for c, _ := range conns {
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
	h.mutex.Lock()

	if channels, ok := h.connsChannels[ws]; ok {
		for _, c := range channels {
			delete(h.channelsConns[c], ws)
		}
	}

	delete(h.connsChannels, ws)

	h.mutex.Unlock()
}
