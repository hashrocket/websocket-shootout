package benchmark

import (
	"encoding/json"
	"errors"
	"log"
	"net"
	"time"
)

type RemoteClientPool struct {
	conn    net.Conn
	encoder *json.Encoder

	clients map[int]*remoteClient
}

type remoteClient struct {
	clientPool *RemoteClientPool
	id         int

	rttResultChan chan time.Duration
	errChan       chan error
}

func NewRemoteClientPool(addr string) (*RemoteClientPool, error) {
	rcp := &RemoteClientPool{}
	rcp.clients = make(map[int]*remoteClient)

	var err error
	rcp.conn, err = net.Dial("tcp", addr)
	if err != nil {
		return nil, err
	}
	rcp.encoder = json.NewEncoder(rcp.conn)

	go rcp.rx()

	return rcp, nil
}

func (rcp *RemoteClientPool) New(
	id int,
	dest, origin, serverType string,
	rttResultChan chan time.Duration,
	errChan chan error,
	padding string,
) (Client, error) {
	client := &remoteClient{
		clientPool:    rcp,
		id:            id,
		rttResultChan: rttResultChan,
		errChan:       errChan,
	}
	rcp.clients[id] = client

	msg := WorkerMsg{
		ClientID: id,
		Type:     "connect",
	}
	msg.Connect = &WorkerConnectMsg{
		Dest:       dest,
		Origin:     origin,
		ServerType: serverType,
		Padding:    padding,
	}

	err := rcp.encoder.Encode(msg)
	if err != nil {
		return nil, err
	}

	return client, nil
}

func (rcp *RemoteClientPool) rx() {
	decoder := json.NewDecoder(rcp.conn)

	for {
		var msg WorkerMsg
		err := decoder.Decode(&msg)
		if err != nil {
			log.Println(err)
			return
		}

		switch msg.Type {
		case "rttResult":
			rcp.clients[msg.ClientID].rttResultChan <- msg.RTTResult.Duration
		case "error":
			rcp.clients[msg.ClientID].errChan <- errors.New(msg.Error.Msg)
		default:
			log.Println("unknown message:", msg.Type)
		}

	}
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
