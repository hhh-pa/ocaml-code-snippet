package measure

// MeasureValue holds one measure value in Bencher Metric Format.
type MeasureValue struct {
	Value any `json:"value"`
}

// Benchmark measures for one snippet.
type Benchmark struct {
	Instructions *MeasureValue `json:"instructions,omitempty"`
	CpuTime      *MeasureValue `json:"cpu_time,omitempty"`
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
		var totalCpu float64

		for _, it := range items {
			s := it.Summary
			if s == nil {
				continue
			}
			if s.Instructions != nil {
				totalInstructions += int64(s.Instructions.Median)
				hasInstructions = true
			}
			if s.CpuS != nil {
				totalCpu += s.CpuS.Median
			}
		}

		bench := Benchmark{
			CpuTime: &MeasureValue{Value: totalCpu},
		}
		if hasInstructions {
			bench.Instructions = &MeasureValue{Value: totalInstructions}
		}
		bmf[label] = bench
	}
	return bmf
}
