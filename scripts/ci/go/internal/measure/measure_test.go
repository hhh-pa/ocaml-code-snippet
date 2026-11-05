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

func TestParsePinCount(t *testing.T) {
	if n, err := ParsePinCount("Count 846018869\n"); err != nil || n != 846018869 {
		t.Fatalf("single line: got %d, %v", n, err)
	}
	if n, err := ParsePinCount("Pin 3.31\nCount 1\nCount 42\n"); err != nil || n != 42 {
		t.Fatalf("later Count must win: got %d, %v", n, err)
	}
	for _, bad := range []string{"", "no counts here\n", "Count abc\n", "Count\n", "Count 1 2\n"} {
		if n, err := ParsePinCount(bad); err == nil {
			t.Fatalf("must reject %q: got %d", bad, n)
		}
	}
}

func TestPinUsableMissing(t *testing.T) {
	if PinUsable("", "") {
		t.Fatal("empty pin paths must be unusable")
	}
	if PinUsable("/nonexistent/pin", "/nonexistent/tool.so") {
		t.Fatal("missing pin files must be unusable")
	}
}

func TestIsPinTimeout(t *testing.T) {
	if !IsPinTimeout("TIMEOUT (>900s)") {
		t.Fatal("timeout not detected")
	}
	for _, other := range []string{"", "exit 1: boom", "pin output unreadable: x", "pin count unreadable: x"} {
		if IsPinTimeout(other) {
			t.Fatalf("false positive: %q", other)
		}
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
