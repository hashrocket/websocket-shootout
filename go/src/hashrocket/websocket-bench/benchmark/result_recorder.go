package benchmark

import (
	"fmt"
	"io"
	"time"
)

type ResultRecorder interface {
	Record(
		clientCount int,
		limitPercentile int,
		rttPercentile time.Duration,
		rttMin time.Duration,
		rttMedian time.Duration,
		rttMax time.Duration,
	) error
}

type TextResultRecorder struct {
	w io.Writer
}

func NewTextResultRecorder(w io.Writer) *TextResultRecorder {
	return &TextResultRecorder{w: w}
}

func (trr *TextResultRecorder) Record(
	clientCount, limitPercentile int,
	rttPercentile, rttMin, rttMedian, rttMax time.Duration,
) error {
	_, err := fmt.Fprintf(trr.w,
		"clients: %5d    %dper-rtt: %3dms    min-rtt: %3dms    median-rtt: %3dms    max-rtt: %3dms\n",
		clientCount,
		limitPercentile,
		roundToMS(rttPercentile),
		roundToMS(rttMin),
		roundToMS(rttMedian),
		roundToMS(rttMax),
	)

	return err
}

func roundToMS(d time.Duration) int64 {
	return int64((d + (500 * time.Microsecond)) / time.Millisecond)
}
