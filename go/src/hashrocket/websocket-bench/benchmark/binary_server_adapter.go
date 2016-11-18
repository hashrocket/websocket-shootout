package benchmark

import (
	"encoding/binary"
	"golang.org/x/net/websocket"
	"time"
)

type BinaryServerAdapter struct {
	conn *websocket.Conn
}

func payloadToBinaryMsg(msgType byte, payload *Payload) []byte {
	// message type - byte (1 byte)
	// payload size - int32 (4 bytes)
	// time sent - int64 (8 bytes)
	// padding - []byte (varies)
	payloadSize := 8 + len(payload.Padding)
	size := 1 + 4 + payloadSize
	buf := make([]byte, size)
	buf[0] = msgType
	binary.BigEndian.PutUint32(buf[1:5], uint32(payloadSize))
	binary.BigEndian.PutUint64(buf[5:13], uint64(payload.SendTime.UnixNano()))
	copy(buf[13:], payload.Padding)
	return buf
}

func (bsa *BinaryServerAdapter) SendEcho(payload *Payload) error {
	return websocket.Message.Send(bsa.conn, payloadToBinaryMsg(MsgClientEcho, payload))
}

func (bsa *BinaryServerAdapter) SendBroadcast(payload *Payload) error {
	return websocket.Message.Send(bsa.conn, payloadToBinaryMsg(MsgClientBroadcast, payload))
}

func (bsa *BinaryServerAdapter) Receive() (*serverSentMsg, error) {
	var buf []byte
	err := websocket.Message.Receive(bsa.conn, &buf)
	if err != nil {
		return nil, err
	}

	var msg serverSentMsg
	msg.Type = buf[0]
	// ignoring payload size as it can be inferred from frame size -- may need to revisit this if messages span frames
	payload := &Payload{
		SendTime: time.Unix(0, int64(binary.BigEndian.Uint64(buf[5:13]))),
	}

	if len(buf) > 13 {
		payload.Padding = buf[13:]
	}

	msg.Payload = payload

	return &msg, nil
}
