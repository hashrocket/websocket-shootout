package main

import (
	"flag"
	"fmt"
	"log"
	"net/url"
	"os"
	"time"
)

func main() {
	var options struct {
		websocketURL       string
		websocketOrigin    string
		clientCount        int
		echoFrequency      int
		broadcastFrequency int
		resetFrequency     int
		statDuration       time.Duration
	}

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "usage:  %s [options]\n", os.Args[0])
		flag.PrintDefaults()
	}

	flag.StringVar(&options.websocketURL, "url", "ws://localhost:3000/ws", "websocket URL")
	flag.StringVar(&options.websocketOrigin, "origin", "", "websocket origin")
	flag.IntVar(&options.clientCount, "clientcount", 1, "number of concurrent clients")
	flag.IntVar(&options.echoFrequency, "echofrequency", 0, "number of echoes per second (distributed among all clients)")
	flag.IntVar(&options.broadcastFrequency, "broadcastfrequency", 0, "number of broadcasts per second (distributed among all clients)")
	flag.IntVar(&options.resetFrequency, "resetfrequency", 0, "number of clients that disconnect and reconnect per second")
	flag.DurationVar(&options.statDuration, "statduration", time.Second*15, "how often to aggregate stats")
	flag.Parse()

	if options.websocketOrigin == "" {
		if wsURL, err := url.Parse(options.websocketURL); err == nil {
			options.websocketOrigin = "http://" + wsURL.Host
		} else {
			log.Fatal(err)
		}
	}

	var echoTickChan, broadcastTickChan, resetTickChan <-chan time.Time
	if options.echoFrequency > 0 {
		echoTickChan = time.Tick(time.Duration(int64(time.Second) / int64(options.echoFrequency)))
	}
	if options.broadcastFrequency > 0 {
		broadcastTickChan = time.Tick(time.Duration(int64(time.Second) / int64(options.broadcastFrequency)))
	}
	if options.resetFrequency > 0 {
		resetTickChan = time.Tick(time.Duration(int64(time.Second) / int64(options.resetFrequency)))
	}

	echoResultChan := make(chan *EchoResult)
	broadcastResultChan := make(chan *BroadcastResult)
	doneChan := make(chan error)

	var runningClients int
	for ; runningClients < options.clientCount; runningClients++ {
		c, err := NewClient(options.websocketURL, options.websocketOrigin, echoTickChan, broadcastTickChan, resetTickChan, echoResultChan, broadcastResultChan, doneChan)
		if err != nil {
			log.Fatal(err)
		}
		go c.Run()
	}

	statTickChan := time.Tick(options.statDuration)
	var echoStat echoStatAggregate
	var broadcastStat broadcastStatAggregate

	for runningClients > 0 {
		select {
		case t := <-statTickChan:
			fmt.Println(t)
			printStats(echoStat, broadcastStat)
			fmt.Println()
			echoStat = echoStatAggregate{}
			broadcastStat = broadcastStatAggregate{}
		case res := <-echoResultChan:
			echoStat.add(res)
		case res := <-broadcastResultChan:
			broadcastStat.add(res)
		case err := <-doneChan:
			runningClients -= 1
			if err != nil {
				log.Println("client died unexpectedly:", err)
			}
		}
	}
}

func printStats(echoStat echoStatAggregate, broadcastStat broadcastStatAggregate) {
	fmt.Println("Echo Count:", echoStat.count)
	if echoStat.count > 0 {
		echoMeanRTT := echoStat.totalRTT / time.Duration(echoStat.count)
		fmt.Printf("Echo RTT: %v min / %v max / %v mean\n", echoStat.minRTT, echoStat.maxRTT, echoMeanRTT)
	}

	fmt.Println("Broadcast Count:", broadcastStat.count)
	if broadcastStat.count > 0 {
		broadcastMeanRTT := broadcastStat.totalRTT / time.Duration(broadcastStat.count)
		fmt.Printf("Broadcast RTT: %v min / %v max / %v mean\n", broadcastStat.minRTT, broadcastStat.maxRTT, broadcastMeanRTT)

		broadcastMeanListeners := broadcastStat.totalListeners / broadcastStat.count
		fmt.Printf("Broadcast Listeners: %v min / %v max / %v mean\n", broadcastStat.minListeners, broadcastStat.maxListeners, broadcastMeanListeners)
	}
}
