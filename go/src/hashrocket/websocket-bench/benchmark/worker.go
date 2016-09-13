package benchmark

import (
	"encoding/json"
	"log"
	"net"
	"strconv"
	"time"
)

type WorkerMsg struct {
	Ref     string            `json:"ref"`
	Type    string            `json:"type"`
	Connect *WorkerConnectMsg `json:"connect,omitempty"`
}

type WorkerConnectMsg struct {
	Dest       string
	Origin     string
	ServerType string
	Padding    string
}

type Worker struct {
	listener net.Listener
	laddr    string

	doneChan      chan error
	rttResultChan chan time.Duration

	clientPools []ClientPool
}

func NewWorker(addr string, port uint16) *Worker {
	w := &Worker{}
	w.laddr = net.JoinHostPort(addr, strconv.FormatInt(int64(port), 10))

	w.doneChan = make(chan error)
	w.rttResultChan = make(chan time.Duration)

	w.clientPools = append(w.clientPools, NewLocalClientPool(nil))

	return w
}

func (w *Worker) Serve() error {
	listener, err := net.Listen("tcp", w.laddr)
	if err != nil {
		return err
	}
	defer listener.Close()

	for {
		conn, err := listener.Accept()
		if err != nil {
			return err
		}

		go w.work(conn)
	}
}

func (w *Worker) work(conn net.Conn) {
	defer conn.Close()

	decoder := json.NewDecoder(conn)
	clientCount := 0

	for {
		var msg WorkerMsg
		err := decoder.Decode(&msg)
		if err != nil {
			log.Println(err)
			return
		}

		switch msg.Type {
		case "connect":
			cp := w.clientPools[clientCount%len(w.clientPools)]
			clientCount++
			_, err := cp.New(msg.Connect.Dest, msg.Connect.Origin, msg.Connect.ServerType, w.rttResultChan, w.doneChan, msg.Connect.Padding)
			if err != nil {
				log.Println(err)
				return
			}
		default:
			log.Println("unknown message:", msg.Type)
		}

	}
}
