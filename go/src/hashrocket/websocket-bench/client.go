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
	serverType    string
	serverAdapter ServerAdapter
	cmdChan       <-chan int
	rxErrChan     chan error
	rttResultChan chan time.Duration
	doneChan      chan error
}

type ServerAdapter interface {
	SendEcho(payload interface{}) error
	SendBroadcast(payload interface{}) error
	Receive() (*serverSentMsg, error)
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
	dest, origin, serverType string,
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

	switch serverType {
	case "standard":
		c.serverAdapter = &StandardServerAdapter{conn: c.conn}
	case "actioncable":
		acsa := &ActionCableServerAdapter{conn: c.conn}
		err = acsa.Startup()
		if err != nil {
			return nil, err
		}
		c.serverAdapter = acsa
	case "phoenix":
		psa := &PhoenixServerAdapter{conn: c.conn}
		err = psa.Startup()
		if err != nil {
			return nil, err
		}
		c.serverAdapter = psa
	default:
		return nil, fmt.Errorf("Unknown server type: %v", serverType)
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
				if err := c.serverAdapter.SendEcho(time.Now().UnixNano()); err != nil {
					panic("SendEcho fail")
				}
			case clientBroadcastCmd:
				if err := c.serverAdapter.SendBroadcast(time.Now().UnixNano()); err != nil {
					panic("SendBroadcast fail")
				}
			case clientResetCmd:
				c.conn.Close()
				<-c.rxErrChan
				if c2, err := NewClient(c.laddr, c.dest, c.origin, c.serverType, c.cmdChan, c.rttResultChan, c.doneChan); err == nil {
					go c2.Run()
				} else {
					c.doneChan <- err
				}
				return
			default:
				panic("unknown cmd")
			}
		case err := <-c.rxErrChan:
			panic("need to rxErrChan")
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
		msg, err := c.serverAdapter.Receive()
		if err != nil {
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
