package main

import (
	"golang.org/x/net/websocket"
)

type StandardServerAdapter struct {
	conn *websocket.Conn
}

type ssaMsg struct {
	Type    string      `json:"type"`
	Payload interface{} `json:"payload"`
}

func (ssa *StandardServerAdapter) SendEcho(payload interface{}) error {
	return websocket.JSON.Send(ssa.conn, &ssaMsg{Type: "echo", Payload: payload})
}

func (ssa *StandardServerAdapter) SendBroadcast(payload interface{}) error {
	return websocket.JSON.Send(ssa.conn, &ssaMsg{Type: "broadcast", Payload: payload})
}

func (ssa *StandardServerAdapter) Receive() (*serverSentMsg, error) {
	var msg serverSentMsg
	err := websocket.JSON.Receive(ssa.conn, &msg)
	return &msg, err
}
