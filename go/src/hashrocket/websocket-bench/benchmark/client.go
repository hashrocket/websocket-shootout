package benchmark

import (
	"fmt"
	"net"
	"strconv"
	"time"

	"golang.org/x/net/websocket"
)

const (
	ClientEchoCmd = iota
	ClientBroadcastCmd
)

type Client interface {
	SendEcho() error
	SendBroadcast() error
}

type localClient struct {
	conn           *websocket.Conn
	config         *websocket.Config
	laddr          *net.TCPAddr
	dest           string
	origin         string
	serverType     string
	serverAdapter  ServerAdapter
	rxErrChan      chan error
	rttResultChan  chan time.Duration
	doneChan       chan error
	payloadPadding string
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
	rttResultChan chan time.Duration,
	doneChan chan error,
	padding string,
) (Client, error) {
	if origin == "" {
		origin = dest
	}

	c := &localClient{
		laddr:          laddr,
		dest:           dest,
		origin:         origin,
		rxErrChan:      make(chan error),
		rttResultChan:  rttResultChan,
		doneChan:       doneChan,
		payloadPadding: padding,
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

	go c.rx()

	return c, nil
}

func (c *localClient) SendEcho() error {
	return c.serverAdapter.SendEcho(&Payload{SendTime: strconv.FormatInt(time.Now().UnixNano(), 10), Padding: c.payloadPadding})
}

func (c *localClient) SendBroadcast() error {
	return c.serverAdapter.SendBroadcast(&Payload{SendTime: strconv.FormatInt(time.Now().UnixNano(), 10), Padding: c.payloadPadding})
}

func (c *localClient) rx() {
	for {
		msg, err := c.serverAdapter.Receive()
		if err != nil {
			panic("todo rxErrChan replacement")
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
					panic("todo rxErrChan replacement")
					c.rxErrChan <- err
				}
			} else {
				panic("todo rxErrChan replacement")
				c.rxErrChan <- fmt.Errorf("received unparsable %s payload: %v", msg.Type, msg.Payload)
			}
		case "broadcast":
		default:
			panic("todo rxErrChan replacement")
			c.rxErrChan <- fmt.Errorf("received unknown message type: %v", msg.Type)
		}
	}
}

type remoteClient struct {
	clientPool *RemoteClientPool
	id         int
}

func (c *remoteClient) SendEcho() error {
	msg := WorkerMsg{
		ClientID: c.id,
		Type:     "echo",
	}

	return c.clientPool.encoder.Encode(msg)
}

func (c *remoteClient) SendBroadcast() error {
	msg := WorkerMsg{
		ClientID: c.id,
		Type:     "broadcast",
	}

	return c.clientPool.encoder.Encode(msg)
}
