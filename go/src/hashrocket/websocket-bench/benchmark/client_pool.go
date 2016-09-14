package benchmark

import (
	"encoding/json"
	"log"
	"net"
	"time"
)

type ClientPool interface {
	New(
		id int,
		dest, origin, serverType string,
		rttResultChan chan time.Duration,
		errChan chan error,
		padding string,
	) (Client, error)
}

type LocalClientPool struct {
	laddr *net.TCPAddr
}

func NewLocalClientPool(laddr *net.TCPAddr) *LocalClientPool {
	return &LocalClientPool{laddr: laddr}
}

func (cf *LocalClientPool) New(
	id int,
	dest, origin, serverType string,
	rttResultChan chan time.Duration,
	errChan chan error,
	padding string,
) (Client, error) {
	c, err := NewClient(cf.laddr, dest, origin, serverType, rttResultChan, errChan, padding)
	if err != nil {
		return nil, err
	}

	return c, nil
}

type RemoteClientPool struct {
	conn    net.Conn
	encoder *json.Encoder

	clients map[int]*remoteClient
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
		default:
			log.Println("unknown message:", msg.Type)
		}

	}
}
