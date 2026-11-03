package measure

import (
	"math"
	"testing"
)

func TestParseCallgrindOutput(t *testing.T) {
	cg := `
events: Ir
summary: 12345678
totals: 12345678
`
	val, err := ParseCallgrindOutput(cg)
	if err != nil || val != 12345678 {
		t.Fatalf("ParseCallgrindOutput: got (%d, %v), want 12345678", val, err)
	}
}

func TestBuildBMF(t *testing.T) {
	results := []CaseResult{
		{
			Label: "sort/bubble_sort",
			Case:  "tc0",
			Summary: &CaseSummary{
				Instructions: &NumberSummary{Median: 500},
				CpuS:         &NumberSummary{Median: 0.02},
			},
		},
		{
			Label: "sort/bubble_sort",
			Case:  "tc1",
			Summary: &CaseSummary{
				Instructions: &NumberSummary{Median: 700},
				CpuS:         &NumberSummary{Median: 0.03},
			},
		},
	}
	bmf := BuildBMF(results)
	bench, ok := bmf["sort/bubble_sort"]
	if !ok {
		t.Fatalf("missing sort/bubble_sort in BMF")
	}
	if bench.Instructions == nil || bench.Instructions.Value.(int64) != 1200 {
		t.Fatalf("instructions = %v, want 1200", bench.Instructions)
	}
	if bench.CpuTime == nil || math.Abs(bench.CpuTime.Value.(float64)-0.05) > 1e-6 {
		t.Fatalf("cpu_time = %v, want 0.05", bench.CpuTime)
	}
}
