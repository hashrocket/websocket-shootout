package benchmark

import (
	"fmt"
	"strconv"
	"time"
)

func ParseMessageType(s string) (byte, error) {
	switch s {
	case "echo":
		return MsgServerEcho, nil
	case "broadcast":
		return MsgServerBroadcast, nil
	case "broadcastResult":
		return MsgServerBroadcastResult, nil
	default:
		return 0, fmt.Errorf("unknown message %s", s)
	}
}

func payloadTojsonPayload(payload *Payload) *jsonPayload {
	sendTime := strconv.FormatInt(payload.SendTime.UnixNano(), 10)
	padding := string(payload.Padding)
	return &jsonPayload{SendTime: sendTime, Padding: padding}
}

func stringToBinaryPayload(strSendTime, strPadding string) (*Payload, error) {
	var payload Payload

	unixNanosecond, err := strconv.ParseInt(strSendTime, 10, 64)
	if err != nil {
		return nil, err
	}
	payload.SendTime = time.Unix(0, unixNanosecond)

	if len(strPadding) > 0 {
		payload.Padding = []byte(strPadding)
	}

	return &payload, nil
}
