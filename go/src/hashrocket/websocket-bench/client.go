package main

import (
	"fmt"
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
	conn                  *websocket.Conn
	config                *websocket.Config
	laddr                 *net.TCPAddr
	dest                  string
	origin                string
	serverType            string
	serverAdapter         ServerAdapter
	cmdChan               <-chan int
	rxErrChan             chan error
	rttResultChan         chan time.Duration
	broadcastReceivedChan chan struct{}
	doneChan              chan error
	payloadPadding        string
}

type ServerAdapter interface {
	SendEcho(payload *Payload) error
	SendBroadcast(payload *Payload) error
	Receive() (*serverSentMsg, error)
}

type Payload struct {
	SendTime string `json:"sendTime"`
	Padding  string `json:"padding,omitempty"`
}

// serverSentMsg includes all fields that can be in server sent message
type serverSentMsg struct {
	Type          string   `json:"type"`
	Payload       *Payload `json:"payload"`
	ListenerCount int      `json:"listenerCount"`
}

func NewClient(
	laddr *net.TCPAddr,
	dest, origin, serverType string,
	cmdChan <-chan int,
	rttResultChan chan time.Duration,
	broadcastReceivedChan chan struct{},
	doneChan chan error,
	padding string,
) (*Client, error) {
	if origin == "" {
		origin = dest
	}

	c := &Client{
		laddr:                 laddr,
		dest:                  dest,
		origin:                origin,
		cmdChan:               cmdChan,
		rxErrChan:             make(chan error),
		rttResultChan:         rttResultChan,
		broadcastReceivedChan: broadcastReceivedChan,
		doneChan:              doneChan,
		payloadPadding:        padding,
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
				if err := c.serverAdapter.SendEcho(&Payload{SendTime: strconv.FormatInt(time.Now().UnixNano(), 10), Padding: c.payloadPadding}); err != nil {
					panic("SendEcho fail")
				}
			case clientBroadcastCmd:
				if err := c.serverAdapter.SendBroadcast(&Payload{SendTime: strconv.FormatInt(time.Now().UnixNano(), 10), Padding: c.payloadPadding}); err != nil {
					panic("SendBroadcast fail")
				}
			case clientResetCmd:
				c.conn.Close()
				<-c.rxErrChan
				if c2, err := NewClient(c.laddr, c.dest, c.origin, c.serverType, c.cmdChan, c.rttResultChan, c.broadcastReceivedChan, c.doneChan, c.payloadPadding); err == nil {
					go c2.Run()
				} else {
					c.doneChan <- err
				}
				return
			default:
				fmt.Println("cmd:", cmd)
				panic("unknown cmd")
			}
		case err := <-c.rxErrChan:
			c.doneChan <- err
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
			if msg.Payload != nil {
				if sentUnixNanosecond, err := strconv.ParseInt(msg.Payload.SendTime, 10, 64); err == nil {
					rtt := time.Duration(time.Now().UnixNano() - int64(sentUnixNanosecond))
					c.rttResultChan <- rtt
				} else {
					c.rxErrChan <- err
				}
			} else {
				c.rxErrChan <- fmt.Errorf("received unparsable %s payload: %v", msg.Type, msg.Payload)
			}
		case "broadcast":
			c.broadcastReceivedChan <- struct{}{}
		default:
			c.rxErrChan <- fmt.Errorf("received unknown message type: %v", msg.Type)
		}
	}
}
