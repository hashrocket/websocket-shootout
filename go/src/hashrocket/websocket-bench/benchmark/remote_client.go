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

	clients            map[int]*remoteClient
	connectSuccessChan chan struct{}
}

type remoteClient struct {
	clientPool *RemoteClientPool
	id         int

	rttResultChan        chan time.Duration
	errChan              chan error
	rxBroadcastCountChan chan int
}

func NewRemoteClientPool(addr string) (*RemoteClientPool, error) {
	rcp := &RemoteClientPool{}
	rcp.clients = make(map[int]*remoteClient)
	rcp.connectSuccessChan = make(chan struct{})

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
	padding []byte,
) (Client, error) {
	client := &remoteClient{
		clientPool:           rcp,
		id:                   id,
		rttResultChan:        rttResultChan,
		errChan:              errChan,
		rxBroadcastCountChan: make(chan int),
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

	<-rcp.connectSuccessChan

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
		case "connect":
			rcp.connectSuccessChan <- struct{}{}
		case "rttResult":
			rcp.clients[msg.ClientID].rttResultChan <- msg.RTTResult.Duration
		case "error":
			rcp.clients[msg.ClientID].errChan <- errors.New(msg.Error.Msg)
		case "rxBroadcastCount":
			rcp.clients[msg.ClientID].rxBroadcastCountChan <- msg.RxBroadcastCount.Count
		default:
			log.Println("unknown message:", msg.Type)
		}

	}
}

func (rcp *RemoteClientPool) Close() error {
	return rcp.conn.Close()
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

func (c *remoteClient) ResetRxBroadcastCount() (int, error) {
	msg := WorkerMsg{
		ClientID: c.id,
		Type:     "resetRxBroadcastCount",
	}

	err := c.clientPool.encoder.Encode(msg)
	if err != nil {
		return 0, err
	}

	count := <-c.rxBroadcastCountChan

	return count, nil
}
