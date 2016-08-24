package main

import (
	"fmt"
	"log"
	"net"
	"os"
	"strings"
	"time"

	"github.com/spf13/cobra"
)

var options struct {
	websocketURL       string
	websocketOrigin    string
	serverType         string
	concurrent         int
	sampleSize         int
	stepSize           int
	limitPercentile    int
	limitRTT           time.Duration
	payloadPaddingSize int
	localAddrs         []string
}

func main() {

	rootCmd := &cobra.Command{Use: "websocket-bench", Short: "websocket benchmark tool"}
	rootCmd.PersistentFlags().StringVarP(&options.websocketOrigin, "origin", "o", "", "websocket origin")
	rootCmd.PersistentFlags().StringSliceVarP(&options.localAddrs, "local-addr", "l", []string{}, "local IP address to connect from")
	rootCmd.PersistentFlags().StringVarP(&options.serverType, "server-type", "", "standard", "server type to connect to (standard, actioncable, phoenix)")

	cmdEcho := &cobra.Command{
		Use:   "echo URL",
		Short: "Echo stress test",
		Long:  "Stress test 1 to 1 performance with an echo test",
		Run:   Stress,
	}
	cmdEcho.Flags().IntVarP(&options.concurrent, "concurrent", "c", 50, "concurrent echo requests")
	cmdEcho.Flags().IntVarP(&options.sampleSize, "sample-size", "s", 10000, "number of echoes in a sample")
	cmdEcho.Flags().IntVarP(&options.stepSize, "step-size", "", 5000, "number of clients to increase each step")
	cmdEcho.Flags().IntVarP(&options.limitPercentile, "limit-percentile", "", 95, "round-trip time percentile to for limit")
	cmdEcho.Flags().IntVarP(&options.payloadPaddingSize, "payload-padding", "", 0, "payload padding size")
	cmdEcho.Flags().DurationVarP(&options.limitRTT, "limit-rtt", "", time.Millisecond*500, "Max RTT at limit percentile")
	rootCmd.AddCommand(cmdEcho)

	cmdBroadcast := &cobra.Command{
		Use:   "broadcast URL",
		Short: "Broadcast stress test",
		Long:  "Stress test 1 to many performance with an broadcast test",
		Run:   Stress,
	}
	cmdBroadcast.Flags().IntVarP(&options.concurrent, "concurrent", "c", 4, "concurrent broadcast requests")
	cmdBroadcast.Flags().IntVarP(&options.sampleSize, "sample-size", "s", 20, "number of broadcasts in a sample")
	cmdBroadcast.Flags().IntVarP(&options.stepSize, "step-size", "", 5000, "number of clients to increase each step")
	cmdBroadcast.Flags().IntVarP(&options.limitPercentile, "limit-percentile", "", 95, "round-trip time percentile to for limit")
	cmdBroadcast.Flags().IntVarP(&options.payloadPaddingSize, "payload-padding", "", 0, "payload padding size")
	cmdBroadcast.Flags().DurationVarP(&options.limitRTT, "limit-rtt", "", time.Millisecond*500, "Max RTT at limit percentile")
	rootCmd.AddCommand(cmdBroadcast)

	rootCmd.Execute()
}

func Stress(cmd *cobra.Command, args []string) {
	if len(args) != 1 {
		cmd.Help()
		os.Exit(1)
	}

	var clientCmd int
	switch cmd.Name() {
	case "echo":
		clientCmd = clientEchoCmd
	case "broadcast":
		clientCmd = clientBroadcastCmd
	default:
		panic("invalid command name")
	}

	options.websocketURL = args[0]
	localAddrs := parseTCPAddrs(options.localAddrs)
	cmdChan := make(chan int)
	rttResultChan := make(chan time.Duration)
	doneChan := make(chan error)

	payloadPadding := strings.Repeat("1234567890", options.payloadPaddingSize/10+1)
	payloadPadding = payloadPadding[:options.payloadPaddingSize]

	clientCount := 0
	for {
		if err := startClients(options.serverType, options.stepSize, localAddrs, cmdChan, rttResultChan, doneChan, payloadPadding); err != nil {
			log.Fatal(err)
		}
		clientCount += options.stepSize

		inProgress := 0
		for i := 0; i < options.concurrent; i++ {
			cmdChan <- clientCmd
			inProgress += 1
		}

		var rttAgg rttAggregate
		for rttAgg.Count() < options.sampleSize {
			select {
			case result := <-rttResultChan:
				rttAgg.Add(result)
				inProgress -= 1
			case err := <-doneChan:
				fmt.Println("doneChan err:", err)
				clientCount--
			}

			if rttAgg.Count()+inProgress < options.sampleSize {
				cmdChan <- clientCmd
				inProgress += 1
			}
		}

		if options.limitRTT < rttAgg.Percentile(options.limitPercentile) {
			return
		}

		fmt.Printf("clients: %5d    %dper-rtt: %3dms    min-rtt: %3dms    median-rtt: %3dms    max-rtt: %3dms\n",
			clientCount,
			options.limitPercentile,
			roundToMS(rttAgg.Percentile(options.limitPercentile)),
			roundToMS(rttAgg.Min()),
			roundToMS(rttAgg.Percentile(50)),
			roundToMS(rttAgg.Max()))
	}
}

func roundToMS(d time.Duration) int64 {
	return int64((d + (500 * time.Microsecond)) / time.Millisecond)
}

func startClients(serverType string, count int, localAddrs []*net.TCPAddr, cmdChan <-chan int, rttResultChan chan time.Duration, doneChan chan error, padding string) error {
	for i := 0; i < count; i++ {
		laddr := localAddrs[i%len(localAddrs)]
		c, err := NewClient(laddr, options.websocketURL, options.websocketOrigin, serverType, cmdChan, rttResultChan, doneChan, padding)
		if err != nil {
			return err
		}
		go c.Run()
	}

	return nil
}

func parseTCPAddrs(stringAddrs []string) []*net.TCPAddr {
	var tcpAddrs []*net.TCPAddr
	for _, s := range stringAddrs {
		tcpAddrs = append(tcpAddrs, &net.TCPAddr{IP: net.ParseIP(s)})
	}

	if len(tcpAddrs) == 0 {
		tcpAddrs = []*net.TCPAddr{nil}
	}

	return tcpAddrs
}
