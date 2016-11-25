package main

import (
	"io"
	"log"
	"sync"

	"golang.org/x/net/websocket"
)

type binaryBenchHandler struct {
	mutex sync.RWMutex
	conns map[*websocket.Conn]struct{}
}

func NewBinaryBenchHandler() *binaryBenchHandler {
	return &binaryBenchHandler{
		conns: make(map[*websocket.Conn]struct{}),
	}
}

func (h *binaryBenchHandler) Accept(ws *websocket.Conn) {
	defer h.cleanup(ws)

	h.mutex.Lock()
	h.conns[ws] = struct{}{}
	h.mutex.Unlock()

	for {
		var buf []byte
		if err := websocket.Message.Receive(ws, &buf); err == io.EOF {
			return
		} else if err != nil {
			log.Println("websocket.Message.Receive err:", err)
			return
		}

		switch buf[0] {
		case 'e':
			if err := h.echo(ws, buf); err != nil {
				log.Println("echo err:", err)
				return
			}
		case 'b':
			if err := h.broadcast(ws, buf); err != nil {
				log.Println("broadcast err:", err)
				return
			}
		default:
			log.Println("unknown message type", buf[0])
			return
		}
	}
}

func (h *binaryBenchHandler) echo(ws *websocket.Conn, rxBuf []byte) error {
	return websocket.Message.Send(ws, rxBuf)
}

func (h *binaryBenchHandler) broadcast(ws *websocket.Conn, rxBuf []byte) error {
	h.mutex.RLock()

	for c := range h.conns {
		if err := websocket.Message.Send(c, rxBuf); err != nil {
			h.mutex.RUnlock()
			return err
		}
	}

	h.mutex.RUnlock()

	resultBuf := make([]byte, len(rxBuf))
	resultBuf[0] = 'r'
	copy(resultBuf[1:], rxBuf[1:])
	return websocket.Message.Send(ws, resultBuf)
}

func (h *binaryBenchHandler) cleanup(ws *websocket.Conn) {
	ws.Close()
	h.mutex.Lock()

	delete(h.conns, ws)

	h.mutex.Unlock()
}
