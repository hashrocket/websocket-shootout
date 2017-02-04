package benchmark

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
)

type Benchmark struct {
	errChan       chan error
	rttResultChan chan time.Duration

	payloadPadding []byte

	Config

	clients []Client
}

type Config struct {
	WebsocketURL       string
	WebsocketOrigin    string
	ServerType         string
	ClientCmd          int
	PayloadPaddingSize int
	InitialClients     int
	StepSize           int
	Concurrent         int
	SampleSize         int
	LimitPercentile    int
	LimitRTT           time.Duration
	TotalSteps         int
	ClientPools        []ClientPool
	ResultRecorder     ResultRecorder
}

func New(config *Config) *Benchmark {
	b := &Benchmark{Config: *config}

	b.errChan = make(chan error)
	b.rttResultChan = make(chan time.Duration)

	b.payloadPadding = []byte(strings.Repeat(
		"1234567890",
		b.PayloadPaddingSize/10+1,
	)[:b.PayloadPaddingSize])

	return b
}

func (b *Benchmark) Run() error {
	var expectedRxBroadcastCount int

	if b.InitialClients == 0 {
		b.InitialClients = b.StepSize
	}
	if err := b.startClients(b.ServerType, b.InitialClients); err != nil {
		return err
	}

	stepNum := 0

	for {

		stepNum += 1
		inProgress := 0
		for i := 0; i < b.Concurrent; i++ {
			if err := b.sendToRandomClient(); err != nil {
				return err
			}
			inProgress++
		}

		var rttAgg rttAggregate
		for rttAgg.Count() < b.SampleSize {
			select {
			case result := <-b.rttResultChan:
				rttAgg.Add(result)
				inProgress--
			case err := <-b.errChan:
				return err
			}

			if rttAgg.Count()+inProgress < b.SampleSize {
				if err := b.sendToRandomClient(); err != nil {
					return err
				}
				inProgress++
			}
		}

		expectedRxBroadcastCount += len(b.clients) * b.SampleSize

		if (b.TotalSteps > 0 && b.TotalSteps == stepNum-1) || (b.TotalSteps == 0 && b.LimitRTT < rttAgg.Percentile(b.LimitPercentile)) {
			if b.ClientCmd == ClientBroadcastCmd {
				// Due to the async nature of the broadcasts and the receptions, it is
				// possible for the broadcastResult to arrive before all the
				// broadcasts. This isn't really a problem when running the benchmark
				// because the samples are will balance each other out. However, it
				// does matter when checking at the end that all expected broadcasts
				// were received. So we wait a little before getting the broadcast
				// count.
				time.Sleep(250 * time.Millisecond)

				totalRxBroadcastCount := 0
				for _, c := range b.clients {
					count, err := c.ResetRxBroadcastCount()
					if err != nil {
						return err
					}
					totalRxBroadcastCount += count
				}
				if totalRxBroadcastCount < expectedRxBroadcastCount {
					return fmt.Errorf("Missing received broadcasts: expected %d, got %d", expectedRxBroadcastCount, totalRxBroadcastCount)
				}
				if totalRxBroadcastCount > expectedRxBroadcastCount {
					return fmt.Errorf("Extra received broadcasts: expected %d, got %d", expectedRxBroadcastCount, totalRxBroadcastCount)
				}
			}

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

		if err := b.startClients(b.ServerType, b.StepSize); err != nil {
			return err
		}
	}
}

func (b *Benchmark) startClients(serverType string, count int) error {
	for i := 0; i < count; i++ {
		cp := b.ClientPools[i%len(b.ClientPools)]
		client, err := cp.New(len(b.clients), b.WebsocketURL, b.WebsocketOrigin, b.ServerType, b.rttResultChan, b.errChan, b.payloadPadding)
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
