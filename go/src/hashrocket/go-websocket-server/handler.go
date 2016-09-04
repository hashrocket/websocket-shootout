package main

import (
	"encoding/json"
	"io"
	"log"
	"sync"

	"golang.org/x/net/websocket"
)

type benchHandler struct {
	mutex sync.RWMutex
	conns map[*websocket.Conn]struct{}
}

type WsMsg struct {
	Type    string      `json:"type"`
	Payload interface{} `json:"payload"`
}

type BroadcastResult struct {
	Type          string      `json:"type"`
	Payload       interface{} `json:"payload"`
	ListenerCount int         `json:"listenerCount"`
}

func NewBenchHandler() *benchHandler {
	return &benchHandler{
		conns: make(map[*websocket.Conn]struct{}),
	}
}

func (h *benchHandler) Accept(ws *websocket.Conn) {
	defer h.cleanup(ws)

	h.mutex.Lock()
	h.conns[ws] = struct{}{}
	h.mutex.Unlock()

	for {
		var msg WsMsg
		if err := websocket.JSON.Receive(ws, &msg); err == io.EOF {
			return
		} else if err != nil {
			log.Println("websocket.JSON.Receive err:", err)
			return
		}

		switch msg.Type {
		case "echo":
			if err := h.echo(ws, msg.Payload); err != nil {
				log.Println("echo err:", err)
				return
			}
		case "broadcast":
			if err := h.broadcast(ws, msg.Payload); err != nil {
				log.Println("broadcast err:", err)
				return
			}
		default:
			log.Println("unknown msg.Type")
			return
		}
	}
}

func (h *benchHandler) echo(ws *websocket.Conn, payload interface{}) error {
	return websocket.JSON.Send(ws, &WsMsg{Type: "echo", Payload: payload})
}

func (h *benchHandler) broadcast(ws *websocket.Conn, payload interface{}) error {
	result := BroadcastResult{Type: "broadcastResult", Payload: payload}

	msg, err := json.Marshal(&WsMsg{Type: "broadcast", Payload: payload})
	if err != nil {
		log.Println("broadcast json marshal err:", err)
	} else {

		h.mutex.RLock()

		for c := range h.conns {
			if err := websocket.Message.Send(c, string(msg)); err == nil {
				result.ListenerCount++
			}
		}
		h.mutex.RUnlock()
	}

	return websocket.JSON.Send(ws, &result)
}

func (h *benchHandler) cleanup(ws *websocket.Conn) {
	ws.Close()
	h.mutex.Lock()

	delete(h.conns, ws)

	h.mutex.Unlock()
}
