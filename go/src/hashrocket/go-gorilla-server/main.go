package main

import (
	"flag"
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"os/signal"
	"runtime/pprof"
	"strings"
	"sync"
	"syscall"
)

func main() {
	var options struct {
		address    string
		port       int
		cpuprofile bool
		memprofile bool
	}

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "usage:  %s [options]\n", os.Args[0])
		flag.PrintDefaults()
	}

	flag.StringVar(&options.address, "address", "localhost", "address to listen on")
	flag.IntVar(&options.port, "port", 3000, "port to listen on")
	flag.BoolVar(&options.cpuprofile, "cpuprofile", false, "capture a cpu profile")
	flag.BoolVar(&options.memprofile, "memprofile", false, "capture a memory profile")
	flag.Parse()

	log.SetFlags(log.LstdFlags | log.Lshortfile)

	if options.cpuprofile {
		out := os.Args[0] + "-cpu.prof"
		var f *os.File
		f, err := os.Create(out)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println("Writing cpu profile to", out)
		pprof.StartCPUProfile(f)
		defer f.Close()
		defer pprof.StopCPUProfile()
	}

	if options.memprofile {
		out := os.Args[0] + "-mem.prof"
		var f *os.File
		f, err := os.Create(out)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println("Writing memory profile to", out)
		pprof.WriteHeapProfile(f)
		defer f.Close()
	}

	wsHandler := NewBenchHandler()
	http.Handle("/ws", wsHandler)

	listenAt := fmt.Sprintf("%s:%d", options.address, options.port)
	log.Printf("Starting to listen on: %s\n", listenAt)

	listener, err := net.Listen("tcp", listenAt)
	if err != nil {
		log.Fatal(err)
	}

	wg := &sync.WaitGroup{}
	wg.Add(1)
	go func() {
		defer wg.Done()
		if err := http.Serve(listener, nil); err != nil && !strings.HasSuffix(err.Error(), "use of closed network connection") {
			log.Println(err)
		}
	}()

	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGINT, syscall.SIGTERM)
	<-c
	listener.Close()
	wg.Wait()
}
