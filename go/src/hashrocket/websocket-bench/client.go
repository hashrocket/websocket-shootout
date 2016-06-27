package main

import (
	"fmt"
	"io"
	"time"

	"golang.org/x/net/websocket"
)

type Client struct {
	conn                *websocket.Conn
	echoTickChan        <-chan time.Time
	broadcastTickChan   <-chan time.Time
	rxErrChan           chan error
	echoResultChan      chan *EchoResult
	broadcastResultChan chan *BroadcastResult
	doneChan            chan error
}

type wsMsg struct {
	Type    string      `json:"type"`
	Payload interface{} `json:"payload"`
}

// serverSentMsg includes all fields that can be in server sent message
type serverSentMsg struct {
	Type         string      `json:"type"`
	Payload      interface{} `json:"payload"`
	SuccessCount int         `json:"successCount"`
	ErrorCount   int         `json:"errorCount"`
}

func NewClient(
	url, origin string,
	echoTickChan, broadcastTickChan <-chan time.Time,
	echoResultChan chan *EchoResult,
	broadcastResultChan chan *BroadcastResult,
	doneChan chan error,
) (*Client, error) {
	c := &Client{
		echoTickChan:        echoTickChan,
		broadcastTickChan:   broadcastTickChan,
		rxErrChan:           make(chan error),
		echoResultChan:      echoResultChan,
		broadcastResultChan: broadcastResultChan,
		doneChan:            doneChan,
	}

	var err error
	c.conn, err = websocket.Dial(url, "", origin)
	if err != nil {
		return nil, err
	}

	return c, nil
}

func (c *Client) Run() {
	go c.rx()

	for {
		select {
		case t := <-c.echoTickChan:
			if err := websocket.JSON.Send(c.conn, &wsMsg{Type: "echo", Payload: t.UnixNano()}); err != nil {
				panic("websocket.JSON.Send fail")
			}
		case t := <-c.broadcastTickChan:
			if err := websocket.JSON.Send(c.conn, &wsMsg{Type: "broadcast", Payload: t.UnixNano()}); err != nil {
				panic("websocket.JSON.Send fail")
			}
		case err := <-c.rxErrChan:
			if err == io.EOF {
				c.doneChan <- nil
			} else {
				c.doneChan <- err
			}
			return
		}
	}
}

func (c *Client) rx() {
	for {
		var msg serverSentMsg
		if err := websocket.JSON.Receive(c.conn, &msg); err != nil {
			c.rxErrChan <- err
			return
		}

		switch msg.Type {
		case "echo":
			if sentUnixNanosecond, ok := msg.Payload.(float64); ok {
				er := &EchoResult{}
				er.RTT = time.Duration(time.Now().UnixNano() - int64(sentUnixNanosecond))
				c.echoResultChan <- er
			} else {
				c.rxErrChan <- fmt.Errorf("received unparsable echo payload: %v", msg.Payload)
			}
		case "broadcast":
		case "broadcastResult":
			if sentUnixNanosecond, ok := msg.Payload.(float64); ok {
				br := &BroadcastResult{}
				br.RTT = time.Duration(time.Now().UnixNano() - int64(sentUnixNanosecond))
				br.SuccessCount = msg.SuccessCount
				br.ErrorCount = msg.ErrorCount
				c.broadcastResultChan <- br
			} else {
				c.rxErrChan <- fmt.Errorf("received unparsable echo payload: %v", msg.Payload)
			}
		default:
			c.rxErrChan <- fmt.Errorf("received unknown message type: %v", msg.Type)
		}
	}
}
