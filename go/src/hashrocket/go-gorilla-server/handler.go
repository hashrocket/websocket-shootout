package main

import (
	"encoding/json"
	"log"
	"net/http"
	"sync"

	"github.com/gorilla/websocket"
)

type benchHandler struct {
	mutex sync.RWMutex
	conns map[*websocket.Conn]*connection
}

type connection struct {
	sync.Mutex
	ws *websocket.Conn
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
		conns: make(map[*websocket.Conn]*connection),
	}
}

var upgrader = websocket.Upgrader{}

func (h *benchHandler) ServeHTTP(rsp http.ResponseWriter, req *http.Request) {
	conn, err := upgrader.Upgrade(rsp, req, nil)
	if err != nil {
		log.Println(err)
		return
	}
	h.Accept(conn)
}

func (h *benchHandler) Accept(ws *websocket.Conn) {
	defer h.cleanup(ws)

	c := &connection{ws: ws}

	h.mutex.Lock()
	h.conns[ws] = c
	h.mutex.Unlock()

	for {
		var msg WsMsg
		err := ws.ReadJSON(&msg)
		if err != nil {
			if err, ok := err.(*websocket.CloseError); ok && err.Code == websocket.CloseAbnormalClosure {
				return
			}
			log.Println("websocket.ReadJSON err:", err)
			return
		}

		switch msg.Type {
		case "echo":
			if err := h.echo(c, msg.Payload); err != nil {
				log.Println("echo err:", err)
				return
			}
		case "broadcast":
			if err := h.broadcast(c, msg.Payload); err != nil {
				log.Println("broadcast err:", err)
				return
			}
		default:
			log.Println("unknown msg.Type")
			return
		}
	}
}

func (h *benchHandler) echo(c *connection, payload interface{}) error {
	c.Lock()
	defer c.Unlock()
	return c.ws.WriteJSON(&WsMsg{Type: "echo", Payload: payload})
}

func (h *benchHandler) broadcast(c *connection, payload interface{}) error {
	result := BroadcastResult{Type: "broadcastResult", Payload: payload}

	msg, err := json.Marshal(&WsMsg{Type: "broadcast", Payload: payload})

	if err != nil {
		log.Println("broadcast json marshal err:", err)
	} else {

		h.mutex.RLock()

		for _, c := range h.conns {
			c.Lock()
			if err := c.ws.WriteMessage(websocket.TextMessage, msg); err == nil {
				result.ListenerCount++
			}
			c.Unlock()
		}
		h.mutex.RUnlock()
	}

	c.Lock()
	defer c.Unlock()
	return c.ws.WriteJSON(&result)
}

func (h *benchHandler) cleanup(ws *websocket.Conn) {
	ws.Close()
	h.mutex.Lock()

	delete(h.conns, ws)

	h.mutex.Unlock()
}
