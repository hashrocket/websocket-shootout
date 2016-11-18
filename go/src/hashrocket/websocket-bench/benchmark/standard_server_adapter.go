package benchmark

import (
	"golang.org/x/net/websocket"
)

type StandardServerAdapter struct {
	conn *websocket.Conn
}

type ssaMsg struct {
	Type    string       `json:"type"`
	Payload *jsonPayload `json:"payload"`
}

func (ssa *StandardServerAdapter) SendEcho(payload *Payload) error {
	return websocket.JSON.Send(ssa.conn, &ssaMsg{Type: "echo", Payload: payloadTojsonPayload(payload)})
}

func (ssa *StandardServerAdapter) SendBroadcast(payload *Payload) error {
	return websocket.JSON.Send(ssa.conn, &ssaMsg{Type: "broadcast", Payload: payloadTojsonPayload(payload)})
}

func (ssa *StandardServerAdapter) Receive() (*serverSentMsg, error) {
	var jsonMsg jsonServerSentMsg
	err := websocket.JSON.Receive(ssa.conn, &jsonMsg)
	if err != nil {
		return nil, err
	}

	var msg serverSentMsg
	msg.Type, err = ParseMessageType(jsonMsg.Type)
	if err != nil {
		return nil, err
	}

	msg.Payload, err = stringToBinaryPayload(jsonMsg.Payload.SendTime, jsonMsg.Payload.Padding)
	if err != nil {
		return nil, err
	}

	return &msg, nil
}
