package benchmark

import (
	"sort"
	"time"
)

type rttAggregate struct {
	samples []time.Duration
	sorted  bool
}

type byAsc []time.Duration

func (a byAsc) Len() int           { return len(a) }
func (a byAsc) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a byAsc) Less(i, j int) bool { return a[i] < a[j] }

func (agg *rttAggregate) Add(rtt time.Duration) {
	agg.samples = append(agg.samples, rtt)
	agg.sorted = false
}

func (agg *rttAggregate) Count() int {
	return len(agg.samples)
}

func (agg *rttAggregate) Min() time.Duration {
	agg.Sort()
	return agg.samples[0]
}

func (agg *rttAggregate) Max() time.Duration {
	agg.Sort()
	return agg.samples[len(agg.samples)-1]
}

func (agg *rttAggregate) Percentile(p int) time.Duration {
	if p <= 0 {
		panic("p must be greater than 0")
	} else if 100 <= p {
		panic("p must be less 100")
	}

	agg.Sort()

	rank := p * len(agg.samples) / 100
	return agg.samples[rank]
}

func (agg *rttAggregate) Sort() {
	if agg.sorted {
		return
	}
	sort.Sort(byAsc(agg.samples))
	agg.sorted = true
}
