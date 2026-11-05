package measure

import (
	"math"
	"os"
	"path/filepath"
	"strings"
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
	if bench.PinInstructions != nil {
		t.Fatalf("pin_instructions = %v, want omitted when no Pin data", bench.PinInstructions)
	}
}

func TestBuildBMFPin(t *testing.T) {
	results := []CaseResult{
		{
			Label: "sort/bubble_sort",
			Case:  "tc0",
			Summary: &CaseSummary{
				PinInstructions: &NumberSummary{Median: 5000},
				CpuS:            &NumberSummary{Median: 0.02},
			},
		},
		{
			Label: "sort/bubble_sort",
			Case:  "tc1",
			Summary: &CaseSummary{
				PinInstructions: &NumberSummary{Median: 7000},
				CpuS:            &NumberSummary{Median: 0.03},
			},
		},
	}
	bmf := BuildBMF(results)
	bench, ok := bmf["sort/bubble_sort"]
	if !ok {
		t.Fatalf("missing sort/bubble_sort in BMF")
	}
	if bench.PinInstructions == nil || bench.PinInstructions.Value.(int64) != 12000 {
		t.Fatalf("pin_instructions = %v, want 12000", bench.PinInstructions)
	}
	if bench.Instructions != nil {
		t.Fatalf("instructions = %v, want omitted when no Callgrind data", bench.Instructions)
	}
}

func TestParseAllocBytes(t *testing.T) {
	if n, found := ParseAllocBytes("SNIP_ALLOCATED_BYTES=12345\n"); !found || n != 12345 {
		t.Fatalf("single marker: got (%d, %v), want (12345, true)", n, found)
	}
	stderr := "some snippet warning\nSNIP_ALLOCATED_BYTES=1\nSNIP_ALLOCATED_BYTES=42\n"
	if n, found := ParseAllocBytes(stderr); !found || n != 42 {
		t.Fatalf("later marker must win: got (%d, %v)", n, found)
	}
	if n, found := ParseAllocBytes("  SNIP_ALLOCATED_BYTES=7  \n"); !found || n != 7 {
		t.Fatalf("surrounding whitespace: got (%d, %v)", n, found)
	}
	for _, absent := range []string{"", "no marker here\n", "SNIP_ALLOCATED_BYTES=abc\n", "SNIP_ALLOCATED_BYTES=\n", "SNIP_ALLOCATED_BYTES=-5\n", "SNIP_ALLOCATED_BYTES=1.5\n"} {
		if n, found := ParseAllocBytes(absent); found {
			t.Fatalf("must not match %q: got %d", absent, n)
		}
	}
	// A malformed marker line never shadows a valid one.
	mixed := "SNIP_ALLOCATED_BYTES=oops\nSNIP_ALLOCATED_BYTES=99\n"
	if n, found := ParseAllocBytes(mixed); !found || n != 99 {
		t.Fatalf("malformed line must be skipped: got (%d, %v)", n, found)
	}
}

func TestBuildBMFPinOnly(t *testing.T) {
	results := []CaseResult{
		{
			Label: "sort/bubble_sort",
			Case:  "tc0",
			Summary: &CaseSummary{
				Runs:            0,
				PinInstructions: &NumberSummary{Median: 5000},
				AllocatedBytes:  &NumberSummary{Median: 800},
			},
		},
		{
			Label: "sort/bubble_sort",
			Case:  "tc1",
			Summary: &CaseSummary{
				Runs:            0,
				PinInstructions: &NumberSummary{Median: 7000},
				AllocatedBytes:  &NumberSummary{Median: 1200},
			},
		},
	}
	bmf := BuildBMF(results)
	bench, ok := bmf["sort/bubble_sort"]
	if !ok {
		t.Fatalf("missing sort/bubble_sort in BMF")
	}
	if bench.PinInstructions == nil || bench.PinInstructions.Value.(int64) != 12000 {
		t.Fatalf("pin_instructions = %v, want 12000", bench.PinInstructions)
	}
	if bench.AllocatedBytes == nil || bench.AllocatedBytes.Value.(int64) != 2000 {
		t.Fatalf("allocated_bytes = %v, want 2000", bench.AllocatedBytes)
	}
	if bench.CpuTime != nil {
		t.Fatalf("cpu_time = %v, want omitted (never zero) when no timed phase ran", bench.CpuTime)
	}
}

func TestBuildBMFPartialAlloc(t *testing.T) {
	results := []CaseResult{
		{
			Label: "sort/bubble_sort",
			Case:  "tc0",
			Summary: &CaseSummary{
				Runs:            0,
				PinInstructions: &NumberSummary{Median: 5000},
				AllocatedBytes:  &NumberSummary{Median: 800},
			},
		},
		{
			Label: "sort/bubble_sort",
			Case:  "tc1",
			Summary: &CaseSummary{
				Runs:            0,
				PinInstructions: &NumberSummary{Median: 7000},
			},
		},
	}
	bmf := BuildBMF(results)
	bench := bmf["sort/bubble_sort"]
	if bench.AllocatedBytes != nil {
		t.Fatalf("allocated_bytes = %v, want omitted when a case lacks the signal", bench.AllocatedBytes)
	}
	if bench.PinInstructions == nil || bench.PinInstructions.Value.(int64) != 12000 {
		t.Fatalf("pin_instructions = %v, want 12000", bench.PinInstructions)
	}
}

func renderSummary(t *testing.T, results []CaseResult) string {
	t.Helper()
	rep := &Report{ToolVersion: ToolVersion, Results: results}
	path := filepath.Join(t.TempDir(), "summary.md")
	if err := AppendSummary(path, rep, BuildBMF(results)); err != nil {
		t.Fatalf("AppendSummary: %v", err)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read summary: %v", err)
	}
	return string(data)
}

func TestAppendSummaryPinOnly(t *testing.T) {
	results := []CaseResult{
		{
			Label: "sort/bubble_sort",
			Case:  "tc0",
			Summary: &CaseSummary{
				Runs:            0,
				PinInstructions: &NumberSummary{Median: 5000},
				AllocatedBytes:  &NumberSummary{Median: 800},
			},
		},
	}
	out := renderSummary(t, results)
	if !strings.Contains(out, "Allocated Bytes") {
		t.Fatalf("summary lacks Allocated Bytes column:\n%s", out)
	}
	if !strings.Contains(out, "| `sort/bubble_sort` | 1 | - | 5,000 | 800 | - |") {
		t.Fatalf("pin-only row must show pin+alloc with dashes for Ir/cpu:\n%s", out)
	}
	if strings.Contains(out, "0.0000") {
		t.Fatalf("pin-only summary must not print zero cpu:\n%s", out)
	}
}

func TestAppendSummaryTimed(t *testing.T) {
	results := []CaseResult{
		{
			Label: "sort/bubble_sort",
			Case:  "tc0",
			Summary: &CaseSummary{
				Runs:            1,
				Instructions:    &NumberSummary{Median: 500},
				PinInstructions: &NumberSummary{Median: 5000},
				AllocatedBytes:  &NumberSummary{Median: 800},
				CpuS:            &NumberSummary{Median: 0.02},
			},
		},
		{
			Label: "sort/bubble_sort",
			Case:  "tc1",
			Summary: &CaseSummary{
				Runs:            1,
				Instructions:    &NumberSummary{Median: 700},
				PinInstructions: &NumberSummary{Median: 7000},
				AllocatedBytes:  &NumberSummary{Median: 1200},
				CpuS:            &NumberSummary{Median: 0.03},
			},
		},
	}
	out := renderSummary(t, results)
	want := "| `sort/bubble_sort` | 2 | 1,200 | 12,000 | 2,000 | 0.0500 |"
	if !strings.Contains(out, want) {
		t.Fatalf("timed row = ?\nwant %s\n got:\n%s", want, out)
	}
}
