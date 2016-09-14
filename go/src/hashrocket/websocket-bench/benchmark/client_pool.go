package benchmark

import (
	"encoding/json"
	"net"
	"time"
)

type ClientPool interface {
	New(
		id int,
		dest, origin, serverType string,
		rttResultChan chan time.Duration,
		doneChan chan error,
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
	doneChan chan error,
	padding string,
) (Client, error) {
	c, err := NewClient(cf.laddr, dest, origin, serverType, rttResultChan, doneChan, padding)
	if err != nil {
		return nil, err
	}

	return c, nil
}

type RemoteClientPool struct {
	conn    net.Conn
	encoder *json.Encoder
}

func NewRemoteClientPool(addr string) (*RemoteClientPool, error) {
	rcp := &RemoteClientPool{}
	var err error
	rcp.conn, err = net.Dial("tcp", addr)
	if err != nil {
		return nil, err
	}
	rcp.encoder = json.NewEncoder(rcp.conn)

	return rcp, nil
}

func (rcp *RemoteClientPool) New(
	id int,
	dest, origin, serverType string,
	rttResultChan chan time.Duration,
	doneChan chan error,
	padding string,
) (Client, error) {
	client := &remoteClient{clientPool: rcp, id: id}

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
