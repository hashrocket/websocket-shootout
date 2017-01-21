package benchmark

import (
	"encoding/json"
	"fmt"
	"golang.org/x/net/websocket"
	"strconv"
	"time"
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

	return nil
}

func (acsa *ActionCableServerAdapter) SendEcho(payload *Payload) error {
	data, err := json.Marshal(map[string]interface{}{"action": "echo", "payload": payloadTojsonPayload(payload)})
	if err != nil {
		return err
	}

	return websocket.JSON.Send(acsa.conn, &acsaMsg{
		Command:    "message",
		Identifier: `{"channel":"BenchmarkChannel"}`,
		Data:       string(data),
	})
}

func (acsa *ActionCableServerAdapter) SendBroadcast(payload *Payload) error {
	data, err := json.Marshal(map[string]interface{}{"action": "broadcast", "payload": payloadTojsonPayload(payload)})
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
	payloadMap := message["payload"].(map[string]interface{})

	payload := &Payload{}
	unixNanosecond, err := strconv.ParseInt(payloadMap["sendTime"].(string), 10, 64)
	if err != nil {
		return nil, err
	}
	payload.SendTime = time.Unix(0, unixNanosecond)

	if padding, ok := payloadMap["padding"]; ok {
		payload.Padding = []byte(padding.(string))
	}

	msgType, err := ParseMessageType(message["action"].(string))
	if err != nil {
		return nil, err
	}

	return &serverSentMsg{Type: msgType, Payload: payload}, nil
}

func (acsa *ActionCableServerAdapter) receiveIgnoringPing() (*acsaMsg, error) {
	for {
		var msg acsaMsg
		err := websocket.JSON.Receive(acsa.conn, &msg)
		if err != nil {
			return nil, err
		}
		// fmt.Printf("acsa %p msg: %#v\n", acsa, msg)
		if msg.Type == "ping" || msg.Type == "confirm_subscription" {
			continue
		}

		return &msg, nil
	}
}
