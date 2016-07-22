package main

import (
	"encoding/json"
	"fmt"
	"golang.org/x/net/websocket"
)

type ActionCableServerAdapter struct {
	conn *websocket.Conn
}

type acsaMsg struct {
	Type       string      `json:"type,omitempty"`
	Command    string      `json:"command,omitempty"`
	Identifier string      `json:"identifier,omitempty"`
	Data       string      `json:"data,omitempty"`
	Message    interface{} `json:"message,omitempty"`
}

func (acsa *ActionCableServerAdapter) Startup() error {
	welcomeMsg, err := acsa.receiveIgnoringPing()
	if err != nil {
		return err
	}
	if welcomeMsg.Type != "welcome" {
		return fmt.Errorf("expected welcome msg, got %v", welcomeMsg)
	}

	err = websocket.JSON.Send(acsa.conn, &acsaMsg{
		Command:    "subscribe",
		Identifier: `{"channel":"BenchmarkChannel"}`,
	})
	if err != nil {
		return err
	}

	confirmSubMsg, err := acsa.receiveIgnoringPing()
	if err != nil {
		return err
	}
	if confirmSubMsg.Type != "confirm_subscription" || confirmSubMsg.Identifier != `{"channel":"BenchmarkChannel"}` {
		return fmt.Errorf("expected confirm_subscription msg, got %v", confirmSubMsg)
	}

	return nil
}

func (acsa *ActionCableServerAdapter) SendEcho(payload interface{}) error {
	data, err := json.Marshal(map[string]interface{}{"action": "echo", "payload": payload})
	if err != nil {
		return err
	}

	return websocket.JSON.Send(acsa.conn, &acsaMsg{
		Command:    "message",
		Identifier: `{"channel":"BenchmarkChannel"}`,
		Data:       string(data),
	})
}

func (acsa *ActionCableServerAdapter) SendBroadcast(payload interface{}) error {
	data, err := json.Marshal(map[string]interface{}{"action": "broadcast", "payload": payload})
	if err != nil {
		return err
	}

	return websocket.JSON.Send(acsa.conn, &acsaMsg{
		Command:    "message",
		Identifier: `{"channel":"BenchmarkChannel"}`,
		Data:       string(data),
	})
}

func (acsa *ActionCableServerAdapter) Receive() (*serverSentMsg, error) {
	msg, err := acsa.receiveIgnoringPing()
	if err != nil {
		return nil, err
	}

	message := msg.Message.(map[string]interface{})

	return &serverSentMsg{Type: message["action"].(string), Payload: message["payload"]}, nil
}

func (acsa *ActionCableServerAdapter) receiveIgnoringPing() (*acsaMsg, error) {
	for {
		var msg acsaMsg
		err := websocket.JSON.Receive(acsa.conn, &msg)
		if err != nil {
			return nil, err
		}
		if msg.Type == "ping" {
			continue
		}

		return &msg, nil
	}
}
