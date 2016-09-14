package benchmark

import (
	"time"
)

const (
	ClientEchoCmd = iota
	ClientBroadcastCmd
)

type Client interface {
	SendEcho() error
	SendBroadcast() error
}

type ClientPool interface {
	New(
		id int,
		dest, origin, serverType string,
		rttResultChan chan time.Duration,
		errChan chan error,
		padding string,
	) (Client, error)
}
