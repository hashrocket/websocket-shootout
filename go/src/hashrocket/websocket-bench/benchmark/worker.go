package benchmark

import (
	"encoding/json"
	"log"
	"net"
	"strconv"
	"time"
)

type WorkerMsg struct {
	ClientID  int                 `json:"clientID"`
	Type      string              `json:"type"`
	Connect   *WorkerConnectMsg   `json:"connect,omitempty"`
	RTTResult *WorkerRTTResultMsg `json:"rttResult,omitempty"`
	Error     *WorkerErrorMsg     `json:"error,omitempty"`
}

type WorkerConnectMsg struct {
	Dest       string
	Origin     string
	ServerType string
	Padding    string
}

type WorkerRTTResultMsg struct {
	Duration time.Duration
}

type WorkerErrorMsg struct {
	Msg string
}

type Worker struct {
	listener net.Listener
	laddr    string
}

type workerConn struct {
	conn        net.Conn
	clientPools []ClientPool
	clients     map[int]Client
}

func NewWorker(addr string, port uint16) *Worker {
	w := &Worker{}
	w.laddr = net.JoinHostPort(addr, strconv.FormatInt(int64(port), 10))

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

		wc := &workerConn{
			conn:        conn,
			clientPools: []ClientPool{NewLocalClientPool(nil)},
			clients:     make(map[int]Client),
		}

		go wc.work()
	}
}

func (wc *workerConn) rx(clientID int, rttResultChan chan time.Duration, errChan chan error) {

	encoder := json.NewEncoder(wc.conn)

	for {
		select {
		case result := <-rttResultChan:
			msg := WorkerMsg{
				ClientID:  clientID,
				Type:      "rttResult",
				RTTResult: &WorkerRTTResultMsg{Duration: result},
			}

			if err := encoder.Encode(msg); err != nil {
				log.Fatalln(err)
			}
		case err := <-errChan:
			msg := WorkerMsg{
				ClientID: clientID,
				Type:     "error",
				Error:    &WorkerErrorMsg{Msg: err.Error()},
			}

			if err := encoder.Encode(msg); err != nil {
				log.Fatalln(err)
			}

			return
		}
	}
}

func (wc *workerConn) work() {
	defer wc.conn.Close()

	decoder := json.NewDecoder(wc.conn)

	for {
		var msg WorkerMsg
		err := decoder.Decode(&msg)
		if err != nil {
			log.Println(err)
			return
		}

		switch msg.Type {
		case "connect":
			cp := wc.clientPools[len(wc.clients)%len(wc.clientPools)]
			rttResultChan := make(chan time.Duration)
			errChan := make(chan error)

			c, err := cp.New(msg.ClientID, msg.Connect.Dest, msg.Connect.Origin, msg.Connect.ServerType, rttResultChan, errChan, msg.Connect.Padding)
			if err != nil {
				log.Println(err)
				return
			}
			wc.clients[msg.ClientID] = c
			go wc.rx(msg.ClientID, rttResultChan, errChan)
		case "echo":
			wc.clients[msg.ClientID].SendEcho()
		case "broadcast":
			wc.clients[msg.ClientID].SendBroadcast()
		default:
			log.Println("unknown message:", msg.Type)
		}

	}
}
