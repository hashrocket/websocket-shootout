package benchmark

import (
	"strings"
	"time"
)

type Benchmark struct {
	cmdChan       chan int
	doneChan      chan error
	rttResultChan chan time.Duration

	payloadPadding string

	Config
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
	ClientFactories    []BenchmarkClientFactory
	ResultRecorder     ResultRecorder
}

type BenchmarkClientFactory interface {
	New(
		dest, origin, serverType string,
		cmdChan <-chan int,
		rttResultChan chan time.Duration,
		doneChan chan error,
		padding string,
	) error
}

func NewBenchmark(config *Config) *Benchmark {
	b := &Benchmark{Config: *config}

	b.cmdChan = make(chan int)
	b.doneChan = make(chan error)
	b.rttResultChan = make(chan time.Duration)

	b.payloadPadding = strings.Repeat(
		"1234567890",
		b.PayloadPaddingSize/10+1,
	)[:b.PayloadPaddingSize]

	return b
}

func (b *Benchmark) Run() error {
	clientCount := 0
	for {
		if err := b.startClients(b.ServerType); err != nil {
			return err
		}
		clientCount += b.StepSize

		inProgress := 0
		for i := 0; i < b.Concurrent; i++ {
			b.cmdChan <- b.ClientCmd
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
				b.cmdChan <- b.ClientCmd
				inProgress += 1
			}
		}

		if b.LimitRTT < rttAgg.Percentile(b.LimitPercentile) {
			return nil
		}

		err := b.ResultRecorder.Record(
			clientCount,
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
		cf := b.ClientFactories[i%len(b.ClientFactories)]
		err := cf.New(b.WebsocketURL, b.WebsocketOrigin, b.ServerType, b.cmdChan, b.rttResultChan, b.doneChan, b.payloadPadding)
		if err != nil {
			return err
		}
	}

	return nil
}
