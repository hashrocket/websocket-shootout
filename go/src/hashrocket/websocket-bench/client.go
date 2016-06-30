package main

import (
	"fmt"
	"io"
	"net"
	"strconv"
	"time"

	"golang.org/x/net/websocket"
)

const (
	clientEchoCmd = iota
	clientBroadcastCmd
	clientResetCmd
)

type Client struct {
	conn          *websocket.Conn
	config        *websocket.Config
	laddr         *net.TCPAddr
	dest          string
	origin        string
	cmdChan       <-chan int
	rxErrChan     chan error
	rttResultChan chan time.Duration
	doneChan      chan error
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
	cmdChan <-chan int,
	rttResultChan chan time.Duration,
	doneChan chan error,
) (*Client, error) {
	if origin == "" {
		origin = dest
	}

	c := &Client{
		laddr:         laddr,
		dest:          dest,
		origin:        origin,
		cmdChan:       cmdChan,
		rxErrChan:     make(chan error),
		rttResultChan: rttResultChan,
		doneChan:      doneChan,
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
		case cmd := <-c.cmdChan:
			switch cmd {
			case clientEchoCmd:
				if err := websocket.JSON.Send(c.conn, &wsMsg{Type: "echo", Payload: time.Now().UnixNano()}); err != nil {
					panic("websocket.JSON.Send fail")
				}
			case clientBroadcastCmd:
				if err := websocket.JSON.Send(c.conn, &wsMsg{Type: "broadcast", Payload: time.Now().UnixNano()}); err != nil {
					panic("websocket.JSON.Send fail")
				}
			case clientResetCmd:
				c.conn.Close()
				<-c.rxErrChan
				if c2, err := NewClient(c.laddr, c.dest, c.origin, c.cmdChan, c.rttResultChan, c.doneChan); err == nil {
					go c2.Run()
				} else {
					c.doneChan <- err
				}
				return
			default:
				panic("unknown cmd")
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
		case "echo", "broadcastResult":
			if sentUnixNanosecond, ok := msg.Payload.(float64); ok {
				rtt := time.Duration(time.Now().UnixNano() - int64(sentUnixNanosecond))
				c.rttResultChan <- rtt
			} else {
				c.rxErrChan <- fmt.Errorf("received unparsable %s payload: %v", msg.Type, msg.Payload)
			}
		case "broadcast":
		default:
			c.rxErrChan <- fmt.Errorf("received unknown message type: %v", msg.Type)
		}
	}
}
