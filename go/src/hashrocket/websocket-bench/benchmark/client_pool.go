package benchmark

import (
	"net"
	"time"
)

type ClientPool interface {
	New(
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
	conn net.Conn
}

func NewRemoteClientPool(addr string) (*RemoteClientPool, error) {
	rcp := &RemoteClientPool{}
	var err error
	rcp.conn, err = net.Dial("tcp", addr)
	if err != nil {
		return nil, err
	}

	return rcp, nil
}

func (rcp *RemoteClientPool) New(
	dest, origin, serverType string,
	rttResultChan chan time.Duration,
	doneChan chan error,
	padding string,
) (Client, error) {

	println("stub establish new remote client")
	return nil, nil
}
