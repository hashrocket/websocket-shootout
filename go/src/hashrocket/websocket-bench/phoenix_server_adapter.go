package main

import (
	"fmt"
	"golang.org/x/net/websocket"
)

type PhoenixServerAdapter struct {
	conn *websocket.Conn
}

type psaMsg struct {
	Topic   string                 `json:"topic"`
	Event   string                 `json:"event"`
	Payload map[string]interface{} `json:"payload"`
	Ref     string                 `json:"ref"`
}

func (psa *PhoenixServerAdapter) Startup() error {
	err := websocket.JSON.Send(psa.conn, &psaMsg{
		Topic:   "room:lobby",
		Event:   "phx_join",
		Payload: map[string]interface{}{},
	})
	if err != nil {
		return err
	}

	var confirmSubMsg psaMsg
	err = websocket.JSON.Receive(psa.conn, &confirmSubMsg)
	if err != nil {
		return err
	}

	if confirmSubMsg.Topic != "room:lobby" || confirmSubMsg.Event != `phx_reply` {
		return fmt.Errorf("expected phx_reply msg, got %v", confirmSubMsg)
	}

	return nil
}

func (psa *PhoenixServerAdapter) SendEcho(payload interface{}) error {
	return websocket.JSON.Send(psa.conn, &psaMsg{
		Topic:   "room:lobby",
		Event:   "echo",
		Payload: map[string]interface{}{"body": payload},
	})
}

func (psa *PhoenixServerAdapter) SendBroadcast(payload interface{}) error {
	return websocket.JSON.Send(psa.conn, &psaMsg{
		Topic:   "room:lobby",
		Event:   "broadcast",
		Payload: map[string]interface{}{"body": payload},
	})
}

func (psa *PhoenixServerAdapter) Receive() (*serverSentMsg, error) {
	var msg psaMsg
	err := websocket.JSON.Receive(psa.conn, &msg)
	if err != nil {
		return nil, err
	}
	if msg.Topic != "room:lobby" {
		return nil, fmt.Errorf("unexpected msg, got %v", msg)
	}

	var payload map[string]interface{}

	if response, ok := msg.Payload["response"].(map[string]interface{}); ok {
		payload = response
	} else {
		payload = msg.Payload
	}

	return &serverSentMsg{Type: payload["type"].(string), Payload: payload["body"]}, nil
}
