package measure

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

// CompareMetric holds one metric's baseline vs current totals plus
// the relative change in percent. A side is nil when that report
// lacks the signal; DeltaPct is nil then, or when Base is zero.
type CompareMetric struct {
	Base     *float64 `json:"base,omitempty"`
	Current  *float64 `json:"current,omitempty"`
	DeltaPct *float64 `json:"delta_pct,omitempty"`
}

// CompareEntry is one snippet's metric deltas. Case counts can
// differ per side when behavior changed between the two commits:
// cases the baseline fails produce no signals there.
type CompareEntry struct {
	Label        string                   `json:"label"`
	BaseCases    int                      `json:"base_cases"`
	CurrentCases int                      `json:"current_cases"`
	Metrics      map[string]CompareMetric `json:"metrics"`
}

// CompareReport compares two measure reports, baseline vs current.
type CompareReport struct {
	ToolVersion int            `json:"tool_version"`
	Baseline    string         `json:"baseline"`
	Current     string         `json:"current"`
	Results     []CompareEntry `json:"results"`
}

// compareMetrics lists BMF metric keys in table order with their
// column titles; cpu_time prints as seconds, the rest as integers.
var compareMetrics = []struct {
	key     string
	title   string
	isFloat bool
}{
	{"instructions", "Instructions (Ir)", false},
	{"pin_instructions", "Pin Instructions", false},
	{"allocated_bytes", "Allocated Bytes", false},
	{"cpu_time", "CPU Time (s)", true},
}

func bmfMetricValue(b Benchmark, key string) *float64 {
	var mv *MeasureValue
	switch key {
	case "instructions":
		mv = b.Instructions
	case "pin_instructions":
		mv = b.PinInstructions
	case "allocated_bytes":
		mv = b.AllocatedBytes
	case "cpu_time":
		mv = b.CpuTime
	}
	if mv == nil {
		return nil
	}
	f, ok := toFloat64(mv.Value)
	if !ok {
		return nil
	}
	return &f
}

func deltaPct(base, cur *float64) *float64 {
	if base == nil || cur == nil || *base == 0 {
		return nil
	}
	d := (*cur - *base) / *base * 100
	return &d
}

// CompareReports diffs two measure reports by snippet label using
// the same summed-median BMF totals the summary table shows.
func CompareReports(base, cur *Report, baseName, curName string) *CompareReport {
	baseBMF := BuildBMF(base.Results)
	curBMF := BuildBMF(cur.Results)
	baseCases := map[string]int{}
	for _, r := range base.Results {
		baseCases[r.Label]++
	}
	curCases := map[string]int{}
	for _, r := range cur.Results {
		curCases[r.Label]++
	}
	labels := map[string]bool{}
	for l := range baseCases {
		labels[l] = true
	}
	for l := range curCases {
		labels[l] = true
	}
	var names []string
	for l := range labels {
		names = append(names, l)
	}
	sort.Strings(names)

	rep := &CompareReport{
		ToolVersion: ToolVersion,
		Baseline:    baseName,
		Current:     curName,
	}
	for _, name := range names {
		entry := CompareEntry{
			Label:        name,
			BaseCases:    baseCases[name],
			CurrentCases: curCases[name],
			Metrics:      map[string]CompareMetric{},
		}
		for _, m := range compareMetrics {
			b := bmfMetricValue(baseBMF[name], m.key)
			c := bmfMetricValue(curBMF[name], m.key)
			if b == nil && c == nil {
				continue
			}
			entry.Metrics[m.key] = CompareMetric{Base: b, Current: c, DeltaPct: deltaPct(b, c)}
		}
		rep.Results = append(rep.Results, entry)
	}
	return rep
}

func compareNum(v float64, isFloat bool) string {
	if isFloat {
		return fmt.Sprintf("%.4f", v)
	}
	return fmtInt(int64(v))
}

// compareCell renders "base -> current (+/-delta%)", tolerating a
// missing side when a snippet gained or lost the signal.
func compareCell(m CompareMetric, isFloat bool) string {
	if m.Base == nil && m.Current == nil {
		return "-"
	}
	if m.Base == nil {
		return fmt.Sprintf("n/a -> %s", compareNum(*m.Current, isFloat))
	}
	if m.Current == nil {
		return fmt.Sprintf("%s -> n/a", compareNum(*m.Base, isFloat))
	}
	if m.DeltaPct == nil {
		return fmt.Sprintf("%s -> %s", compareNum(*m.Base, isFloat), compareNum(*m.Current, isFloat))
	}
	return fmt.Sprintf("%s -> %s (%+.2f%%)",
		compareNum(*m.Base, isFloat), compareNum(*m.Current, isFloat), *m.DeltaPct)
}

// AppendCompareSummary appends the per-snippet delta table to path.
func AppendCompareSummary(path string, rep *CompareReport) error {
	if path == "" {
		return nil
	}
	var cols []struct {
		key     string
		title   string
		isFloat bool
	}
	for _, m := range compareMetrics {
		for _, e := range rep.Results {
			if _, ok := e.Metrics[m.key]; ok {
				cols = append(cols, m)
				break
			}
		}
	}

	lines := []string{
		fmt.Sprintf("### Performance Comparison (OCaml): `%s` -> `%s`", rep.Baseline, rep.Current),
		"",
	}
	header := "| Benchmark |"
	sep := "| :--- |"
	for _, c := range cols {
		header += " " + c.title + " |"
		sep += " :---: |"
	}
	lines = append(lines, header, sep)

	totals := map[string]*CompareMetric{}
	for _, c := range cols {
		totals[c.key] = &CompareMetric{}
	}
	for _, e := range rep.Results {
		row := fmt.Sprintf("| `%s` |", e.Label)
		for _, c := range cols {
			m, ok := e.Metrics[c.key]
			if !ok {
				m = CompareMetric{}
			}
			row += " " + compareCell(m, c.isFloat) + " |"
			t := totals[c.key]
			if m.Base != nil {
				if t.Base == nil {
					t.Base = new(float64)
				}
				*t.Base += *m.Base
			}
			if m.Current != nil {
				if t.Current == nil {
					t.Current = new(float64)
				}
				*t.Current += *m.Current
			}
		}
		lines = append(lines, row)
	}
	for _, c := range cols {
		t := totals[c.key]
		t.DeltaPct = deltaPct(t.Base, t.Current)
	}
	total := fmt.Sprintf("| **Total (%d snippets)** |", len(rep.Results))
	for _, c := range cols {
		total += " **" + compareCell(*totals[c.key], c.isFloat) + "** |"
	}
	lines = append(lines, total, "")

	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o666)
	if err != nil {
		return err
	}
	defer f.Close()
	_, err = f.WriteString(strings.Join(lines, "\n") + "\n")
	return err
}
