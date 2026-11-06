package measure

// MeasureValue holds one measure value in Bencher Metric Format.
type MeasureValue struct {
	Value any `json:"value"`
}

// Benchmark measures for one snippet. Instructions is the Callgrind Ir
// total (--callgrind runs; CI dispatches install valgrind for
// those); PinInstructions is the Pin inscount total (--pin runs)
// and AllocatedBytes the Gc.allocated_bytes total from the same
// Pin runs. CpuTime is omitted when no timed phase ran
// (pin-only sweeps), so BMF never carries zero cpu_time rows.
type Benchmark struct {
	Instructions    *MeasureValue `json:"instructions,omitempty"`
	PinInstructions *MeasureValue `json:"pin_instructions,omitempty"`
	AllocatedBytes  *MeasureValue `json:"allocated_bytes,omitempty"`
	CpuTime         *MeasureValue `json:"cpu_time,omitempty"`
}

// BMF maps snippet benchmark label to measures.
type BMF map[string]Benchmark

// BuildBMF aggregates CaseResult entries by snippet label into Bencher Metric Format.
func BuildBMF(results []CaseResult) BMF {
	snippets := map[string][]CaseResult{}
	for _, r := range results {
		snippets[r.Label] = append(snippets[r.Label], r)
	}

	bmf := BMF{}
	for label, items := range snippets {
		var totalInstructions int64
		hasInstructions := false
		var totalPin int64
		hasPin := false
		var totalAlloc int64
		allocCases := 0
		var totalCpu float64
		hasCpu := false

		for _, it := range items {
			s := it.Summary
			if s == nil {
				continue
			}
			if s.Instructions != nil {
				totalInstructions += int64(s.Instructions.Median)
				hasInstructions = true
			}
			if s.PinInstructions != nil {
				totalPin += int64(s.PinInstructions.Median)
				hasPin = true
			}
			if s.AllocatedBytes != nil {
				totalAlloc += int64(s.AllocatedBytes.Median)
				allocCases++
			}
			if s.CpuS != nil {
				totalCpu += s.CpuS.Median
				hasCpu = true
			}
		}

		bench := Benchmark{}
		if hasInstructions {
			bench.Instructions = &MeasureValue{Value: totalInstructions}
		}
		if hasPin {
			bench.PinInstructions = &MeasureValue{Value: totalPin}
		}
		if allocCases == len(items) && len(items) > 0 {
			bench.AllocatedBytes = &MeasureValue{Value: totalAlloc}
		}
		if hasCpu {
			bench.CpuTime = &MeasureValue{Value: totalCpu}
		}
		bmf[label] = bench
	}
	return bmf
}
