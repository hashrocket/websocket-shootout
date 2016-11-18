package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"

	"golang.org/x/net/websocket"
)

func main() {
	var options struct {
		address string
		port    int
	}

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "usage:  %s [options]\n", os.Args[0])
		flag.PrintDefaults()
	}

	flag.StringVar(&options.address, "address", "localhost", "address to listen on")
	flag.IntVar(&options.port, "port", 3000, "port to listen on")
	flag.Parse()

	wsJSONHandler := NewJSONBenchHandler()
	wsJSONServer := websocket.Server{Handler: wsJSONHandler.Accept}
	http.Handle("/json", wsJSONServer)

	wsBinaryHandler := NewBinaryBenchHandler()
	wsBinaryServer := websocket.Server{Handler: wsBinaryHandler.Accept}
	http.Handle("/binary", wsBinaryServer)

	listenAt := fmt.Sprintf("%s:%d", options.address, options.port)
	log.Printf("Starting to listen on: %s\n", listenAt)

	if err := http.ListenAndServe(listenAt, nil); err != nil {
		log.Fatalf("Could not start web server: %v\n", err)
	}
}
