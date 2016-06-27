package main

import (
	"io"
	"log"
	"sync"

	"golang.org/x/net/websocket"
)

type benchHandler struct {
	mutex sync.Mutex
	conns map[*websocket.Conn]struct{}
}

type WsMsg struct {
	Type    string      `json:"type"`
	Payload interface{} `json:"payload"`
}

type BroadcastResult struct {
	Type         string      `json:"type"`
	Payload      interface{} `json:"payload"`
	SuccessCount int         `json:"successCount"`
	ErrorCount   int         `json:"errorCount"`
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

	h.mutex.Lock()

	for c, _ := range h.conns {
		if c != ws {
			if err := websocket.JSON.Send(c, &WsMsg{Type: "broadcast", Payload: payload}); err == nil {
				result.SuccessCount += 1
			} else {
				result.ErrorCount += 1
				log.Println("websocket.JSON.Send err:", err)
			}
		}
	}

	h.mutex.Unlock()

	return websocket.JSON.Send(ws, &result)
}

func (h *benchHandler) cleanup(ws *websocket.Conn) {
	ws.Close()
	h.mutex.Lock()

	delete(h.conns, ws)

	h.mutex.Unlock()
}
