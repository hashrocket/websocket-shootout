package benchmark

import (
	"math/rand"
	"strings"
	"time"
)

type Benchmark struct {
	doneChan      chan error
	rttResultChan chan time.Duration

	payloadPadding string

	Config

	clients []Client
}

type Config struct {
	WebsocketURL       string
	WebsocketOrigin    string
	ServerType         string
	ClientCmd          int
	PayloadPaddingSize int
	StepSize           int
	Concurrent         int
	SampleSize         int
	LimitPercentile    int
	LimitRTT           time.Duration
	ClientPools        []ClientPool
	ResultRecorder     ResultRecorder
}

func New(config *Config) *Benchmark {
	b := &Benchmark{Config: *config}

	b.doneChan = make(chan error)
	b.rttResultChan = make(chan time.Duration)

	b.payloadPadding = strings.Repeat(
		"1234567890",
		b.PayloadPaddingSize/10+1,
	)[:b.PayloadPaddingSize]

	return b
}

func (b *Benchmark) Run() error {
	for {
		if err := b.startClients(b.ServerType); err != nil {
			return err
		}

		inProgress := 0
		for i := 0; i < b.Concurrent; i++ {
			if err := b.sendToRandomClient(); err != nil {
				return err
			}
			inProgress += 1
		}

		var rttAgg rttAggregate
		for rttAgg.Count() < b.SampleSize {
			select {
			case result := <-b.rttResultChan:
				rttAgg.Add(result)
				inProgress -= 1
			case err := <-b.doneChan:
				return err
			}

			if rttAgg.Count()+inProgress < b.SampleSize {
				if err := b.sendToRandomClient(); err != nil {
					return err
				}
				inProgress += 1
			}
		}

		if b.LimitRTT < rttAgg.Percentile(b.LimitPercentile) {
			return nil
		}

		err := b.ResultRecorder.Record(
			len(b.clients),
			b.LimitPercentile,
			rttAgg.Percentile(b.LimitPercentile),
			rttAgg.Min(),
			rttAgg.Percentile(50),
			rttAgg.Max(),
		)
		if err != nil {
			return err
		}
	}
}

func (b *Benchmark) startClients(serverType string) error {
	for i := 0; i < b.StepSize; i++ {
		cp := b.ClientPools[i%len(b.ClientPools)]
		client, err := cp.New(len(b.clients), b.WebsocketURL, b.WebsocketOrigin, b.ServerType, b.rttResultChan, b.doneChan, b.payloadPadding)
		if err != nil {
			return err
		}
		b.clients = append(b.clients, client)
	}

	return nil
}

func (b *Benchmark) randomClient() Client {
	if len(b.clients) == 0 {
		panic("no clients")
	}

	return b.clients[rand.Intn(len(b.clients))]
}

func (b *Benchmark) sendToRandomClient() error {
	if len(b.clients) == 0 {
		panic("no clients")
	}

	client := b.randomClient()
	switch b.ClientCmd {
	case ClientEchoCmd:
		if err := client.SendEcho(); err != nil {
			return err
		}
	case ClientBroadcastCmd:
		if err := client.SendBroadcast(); err != nil {
			return err
		}
	default:
		panic("unknown client command")
	}

	return nil
}
