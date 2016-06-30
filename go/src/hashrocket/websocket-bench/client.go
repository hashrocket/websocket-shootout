package main

import (
	"fmt"
	"io"
	"net"
	"strconv"
	"time"

	"golang.org/x/net/websocket"
)

type Client struct {
	conn                *websocket.Conn
	config              *websocket.Config
	laddr               *net.TCPAddr
	dest                string
	origin              string
	echoTickChan        <-chan time.Time
	broadcastTickChan   <-chan time.Time
	resetTickChan       <-chan time.Time
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
	Type          string      `json:"type"`
	Payload       interface{} `json:"payload"`
	ListenerCount int         `json:"listenerCount"`
}

func NewClient(
	laddr *net.TCPAddr,
	dest, origin string,
	echoTickChan, broadcastTickChan, resetTickChan <-chan time.Time,
	echoResultChan chan *EchoResult,
	broadcastResultChan chan *BroadcastResult,
	doneChan chan error,
) (*Client, error) {
	c := &Client{
		laddr:               laddr,
		dest:                dest,
		origin:              origin,
		echoTickChan:        echoTickChan,
		broadcastTickChan:   broadcastTickChan,
		resetTickChan:       resetTickChan,
		rxErrChan:           make(chan error),
		echoResultChan:      echoResultChan,
		broadcastResultChan: broadcastResultChan,
		doneChan:            doneChan,
	}

	config, err := websocket.NewConfig(dest, origin)
	if err != nil {
		return nil, err
	}

	host, port, err := net.SplitHostPort(config.Location.Host)
	if err != nil {
		return nil, err
	}

	destIPs, err := net.LookupHost(host)
	if err != nil {
		return nil, err
	}

	nport, err := strconv.ParseUint(port, 10, 16)
	if err != nil {
		return nil, err
	}

	raddr := net.TCPAddr{IP: net.ParseIP(destIPs[0]), Port: int(nport)}
	tcpConn, err := net.DialTCP("tcp", c.laddr, &raddr)
	if err != nil {
		return nil, err
	}

	c.conn, err = websocket.NewClient(config, tcpConn)
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
		case <-c.resetTickChan:
			c.conn.Close()
			<-c.rxErrChan
			if c2, err := NewClient(c.laddr, c.dest, c.origin, c.echoTickChan, c.broadcastTickChan, c.resetTickChan, c.echoResultChan, c.broadcastResultChan, c.doneChan); err == nil {
				go c2.Run()
			} else {
				c.doneChan <- err
			}
			return
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
				br.ListenerCount = msg.ListenerCount
				c.broadcastResultChan <- br
			} else {
				c.rxErrChan <- fmt.Errorf("received unparsable echo payload: %v", msg.Payload)
			}
		default:
			c.rxErrChan <- fmt.Errorf("received unknown message type: %v", msg.Type)
		}
	}
}
