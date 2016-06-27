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

	maxSuccessSends   int
	minSuccessSends   int
	totalSuccessSends int

	maxErrorSends   int
	minErrorSends   int
	totalErrorSends int
}

type EchoResult struct {
	RTT time.Duration // Round-trip time
}

type BroadcastResult struct {
	RTT          time.Duration // Round-trip time
	SuccessCount int
	ErrorCount   int
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

	if agg.minSuccessSends == 0 || res.SuccessCount < agg.minSuccessSends {
		agg.minSuccessSends = res.SuccessCount
	}
	if res.SuccessCount > agg.maxSuccessSends {
		agg.maxSuccessSends = res.SuccessCount
	}
	agg.totalSuccessSends += res.SuccessCount

	if agg.minErrorSends == 0 || res.ErrorCount < agg.minErrorSends {
		agg.minErrorSends = res.ErrorCount
	}
	if res.ErrorCount > agg.maxErrorSends {
		agg.maxErrorSends = res.ErrorCount
	}
	agg.totalErrorSends += res.ErrorCount
}
