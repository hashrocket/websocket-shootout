package main

import (
	"time"
)

type echoStatAggregate struct {
	count int

	maxRTT   time.Duration
	minRTT   time.Duration
	totalRTT time.Duration
}

type broadcastStatAggregate struct {
	count int

	maxRTT   time.Duration
	minRTT   time.Duration
	totalRTT time.Duration

	maxListeners   int
	minListeners   int
	totalListeners int
}

type EchoResult struct {
	RTT time.Duration // Round-trip time
}

type BroadcastResult struct {
	RTT           time.Duration // Round-trip time
	ListenerCount int
}

func (agg *echoStatAggregate) add(res *EchoResult) {
	agg.count += 1

	if agg.minRTT == 0 || res.RTT < agg.minRTT {
		agg.minRTT = res.RTT
	}
	if res.RTT > agg.maxRTT {
		agg.maxRTT = res.RTT
	}
	agg.totalRTT += res.RTT
}

func (agg *broadcastStatAggregate) add(res *BroadcastResult) {
	agg.count += 1

	if agg.minRTT == 0 || res.RTT < agg.minRTT {
		agg.minRTT = res.RTT
	}
	if res.RTT > agg.maxRTT {
		agg.maxRTT = res.RTT
	}
	agg.totalRTT += res.RTT

	if agg.minListeners == 0 || res.ListenerCount < agg.minListeners {
		agg.minListeners = res.ListenerCount
	}
	if res.ListenerCount > agg.maxListeners {
		agg.maxListeners = res.ListenerCount
	}
	agg.totalListeners += res.ListenerCount
}
