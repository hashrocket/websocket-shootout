package benchmark

import (
	"fmt"
	"golang.org/x/net/websocket"
	"strconv"
	"time"
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
		Topic: "room:lobby",
		Event: "phx_join",
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

func (psa *PhoenixServerAdapter) SendEcho(payload *Payload) error {
	return websocket.JSON.Send(psa.conn, &psaMsg{
		Topic:   "room:lobby",
		Event:   "echo",
		Payload: map[string]interface{}{"body": payload},
	})
}

func (psa *PhoenixServerAdapter) SendBroadcast(payload *Payload) error {
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

	var psaPayload map[string]interface{}

	if response, ok := msg.Payload["response"].(map[string]interface{}); ok {
		psaPayload = response
	} else {
		psaPayload = msg.Payload
	}

	body := psaPayload["body"].(map[string]interface{})

	payload := &Payload{}
	unixNanosecond, err := strconv.ParseInt(body["sendTime"].(string), 10, 64)
	if err != nil {
		return nil, err
	}
	payload.SendTime = time.Unix(0, unixNanosecond)

	if padding, ok := body["padding"]; ok {
		payload.Padding = []byte(padding.(string))
	}

	msgType, err := ParseMessageType(psaPayload["type"].(string))
	if err != nil {
		return nil, err
	}

	return &serverSentMsg{Type: msgType, Payload: payload}, nil
}
