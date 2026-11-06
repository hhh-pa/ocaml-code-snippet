package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func sampleTestStats() []testCaseStats {
	return []testCaseStats{
		{Label: "sort/bubble_sort", Case: "tc1", Passed: true, WallS: 0.03, CpuS: 0.03, MaxRSSKb: 2000},
		{Label: "sort/bubble_sort", Case: "tc0", Passed: true, WallS: 0.02, CpuS: 0.02, MaxRSSKb: 1000},
		{Label: "math/fib", Case: "tc0", Passed: false},
	}
}

func TestWriteTestStatsJSON(t *testing.T) {
	path := filepath.Join(t.TempDir(), "stats.json")
	if err := writeTestStatsJSON(path, sampleTestStats()); err != nil {
		t.Fatalf("writeTestStatsJSON: %v", err)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	var got []testCaseStats
	if err := json.Unmarshal(data, &got); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if len(got) != 3 {
		t.Fatalf("cases = %d, want 3", len(got))
	}
	if got[0].Label != "sort/bubble_sort" || got[0].Case != "tc1" || !got[0].Passed {
		t.Fatalf("first entry = %+v, want bubble_sort/tc1 passed", got[0])
	}
	if got[0].MaxRSSKb != 2000 {
		t.Fatalf("maxrss = %d, want 2000", got[0].MaxRSSKb)
	}
	if got[2].Passed || got[2].CpuS != 0 {
		t.Fatalf("failed entry = %+v, want zeros", got[2])
	}
}

func TestAppendTestStatsSummary(t *testing.T) {
	path := filepath.Join(t.TempDir(), "summary.md")
	if err := appendTestStatsSummary(path, " [shard 0/4]", sampleTestStats()); err != nil {
		t.Fatalf("appendTestStatsSummary: %v", err)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	out := string(data)
	if !strings.Contains(out, "### Test Resource Stats (OCaml) [shard 0/4]") {
		t.Fatalf("title lacks shard suffix:\n%s", out)
	}
	if !strings.Contains(out, "| `sort/bubble_sort` | 2 | 0.0500 | 2000 |") {
		t.Fatalf("snippet row wrong:\n%s", out)
	}
	if !strings.Contains(out, "| `math/fib` | 1 | 0.0000 | 0 |") {
		t.Fatalf("failed-snippet row wrong:\n%s", out)
	}
	if !strings.Contains(out, "| **Total (2 snippets)** | 3 | **0.0500** | **2000** |") {
		t.Fatalf("total row wrong:\n%s", out)
	}
}

func TestAppendTestStatsSummaryNoPath(t *testing.T) {
	if err := appendTestStatsSummary("", "", sampleTestStats()); err != nil {
		t.Fatalf("empty path must be a no-op: %v", err)
	}
}
