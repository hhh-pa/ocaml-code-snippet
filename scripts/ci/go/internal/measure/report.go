package measure

import (
	"encoding/json"
	"os"
	"runtime"
	"sort"
	"time"
)

// NumberSummary holds statistical summary of numeric measurements.
type NumberSummary struct {
	Min    float64 `json:"min"`
	Median float64 `json:"median"`
	Max    float64 `json:"max"`
}

// CaseSummary holds per-case aggregate measurements across repeats.
type CaseSummary struct {
	Instructions *NumberSummary `json:"instructions"`
	WallS        *NumberSummary `json:"wall_s"`
	CpuS         *NumberSummary `json:"cpu_s"`
	MaxRSSKb     *NumberSummary `json:"maxrss_kb"`
	Runs         int            `json:"runs"`
}

// CaseResult is one measured test case.
type CaseResult struct {
	Label   string       `json:"label"`
	Stem    string       `json:"stem"`
	Case    string       `json:"case"`
	Script  string       `json:"script"`
	Summary *CaseSummary `json:"summary"`
}

// Report is the full JSON output of snip measure.
type Report struct {
	ToolVersion int          `json:"tool_version"`
	Platform    string       `json:"platform"`
	Date        string       `json:"date"`
	HasValgrind bool         `json:"has_valgrind"`
	Results     []CaseResult `json:"results"`
	Failures    []string     `json:"failures"`
}

// SummarizeNumbers computes min, median, max for a slice of floats.
func SummarizeNumbers(vals []float64) *NumberSummary {
	if len(vals) == 0 {
		return nil
	}
	sorted := make([]float64, len(vals))
	copy(sorted, vals)
	sort.Float64s(sorted)
	n := len(sorted)
	var median float64
	if n%2 == 1 {
		median = sorted[n/2]
	} else {
		median = (sorted[n/2-1] + sorted[n/2]) / 2.0
	}
	return &NumberSummary{
		Min:    sorted[0],
		Median: median,
		Max:    sorted[n-1],
	}
}

// MergeReports combines multiple shard Report JSON files into one.
func MergeReports(paths []string) (*Report, BMF, error) {
	var mergedResults []CaseResult
	var failures []string
	seen := map[string]bool{}

	sortedPaths := make([]string, len(paths))
	copy(sortedPaths, paths)
	sort.Strings(sortedPaths)

	for _, p := range sortedPaths {
		data, err := os.ReadFile(p)
		if err != nil {
			return nil, nil, err
		}
		var rep Report
		if err := json.Unmarshal(data, &rep); err != nil {
			return nil, nil, err
		}
		for _, item := range rep.Results {
			key := item.Label + "/" + item.Case
			if !seen[key] {
				seen[key] = true
				mergedResults = append(mergedResults, item)
			}
		}
		failures = append(failures, rep.Failures...)
	}

	sort.Slice(mergedResults, func(i, j int) bool {
		if mergedResults[i].Label != mergedResults[j].Label {
			return mergedResults[i].Label < mergedResults[j].Label
		}
		return mergedResults[i].Case < mergedResults[j].Case
	})

	rep := &Report{
		ToolVersion: 1,
		Platform:    runtime.GOOS + "/" + runtime.GOARCH,
		Date:        time.Now().UTC().Format(time.RFC3339),
		HasValgrind: true,
		Results:     mergedResults,
		Failures:    failures,
	}
	return rep, BuildBMF(mergedResults), nil
}
