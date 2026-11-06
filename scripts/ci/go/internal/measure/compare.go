package measure

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

// CompareMetric holds one metric's from vs to totals plus the
// relative change in percent. A side is nil when that report
// lacks the signal; DeltaPct is nil then, or when From is zero.
type CompareMetric struct {
	From     *float64 `json:"from,omitempty"`
	To       *float64 `json:"to,omitempty"`
	DeltaPct *float64 `json:"delta_pct,omitempty"`
}

// CompareEntry is one snippet's metric deltas. Case counts can
// differ per side when behavior changed between the two commits:
// cases the from-tree fails produce no signals there.
type CompareEntry struct {
	Label     string                   `json:"label"`
	FromCases int                      `json:"from_cases"`
	ToCases   int                      `json:"to_cases"`
	Metrics   map[string]CompareMetric `json:"metrics"`
}

// CompareReport compares two measure reports, from vs to.
type CompareReport struct {
	ToolVersion int            `json:"tool_version"`
	From        string         `json:"from"`
	To          string         `json:"to"`
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

func deltaPct(from, to *float64) *float64 {
	if from == nil || to == nil || *from == 0 {
		return nil
	}
	d := (*to - *from) / *from * 100
	return &d
}

// CompareReports diffs two measure reports by snippet label using
// the same summed-median BMF totals the summary table shows.
func CompareReports(from, to *Report, fromName, toName string) *CompareReport {
	fromBMF := BuildBMF(from.Results)
	toBMF := BuildBMF(to.Results)
	fromCases := map[string]int{}
	for _, r := range from.Results {
		fromCases[r.Label]++
	}
	toCases := map[string]int{}
	for _, r := range to.Results {
		toCases[r.Label]++
	}
	labels := map[string]bool{}
	for l := range fromCases {
		labels[l] = true
	}
	for l := range toCases {
		labels[l] = true
	}
	var names []string
	for l := range labels {
		names = append(names, l)
	}
	sort.Strings(names)

	rep := &CompareReport{
		ToolVersion: ToolVersion,
		From:        fromName,
		To:          toName,
	}
	for _, name := range names {
		entry := CompareEntry{
			Label:     name,
			FromCases: fromCases[name],
			ToCases:   toCases[name],
			Metrics:   map[string]CompareMetric{},
		}
		for _, m := range compareMetrics {
			f := bmfMetricValue(fromBMF[name], m.key)
			t := bmfMetricValue(toBMF[name], m.key)
			if f == nil && t == nil {
				continue
			}
			entry.Metrics[m.key] = CompareMetric{From: f, To: t, DeltaPct: deltaPct(f, t)}
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

// compareCell renders "from -> to (+/-delta%)", tolerating a
// missing side when a snippet gained or lost the signal.
func compareCell(m CompareMetric, isFloat bool) string {
	if m.From == nil && m.To == nil {
		return "-"
	}
	if m.From == nil {
		return fmt.Sprintf("n/a -> %s", compareNum(*m.To, isFloat))
	}
	if m.To == nil {
		return fmt.Sprintf("%s -> n/a", compareNum(*m.From, isFloat))
	}
	if m.DeltaPct == nil {
		return fmt.Sprintf("%s -> %s", compareNum(*m.From, isFloat), compareNum(*m.To, isFloat))
	}
	return fmt.Sprintf("%s -> %s (%+.2f%%)",
		compareNum(*m.From, isFloat), compareNum(*m.To, isFloat), *m.DeltaPct)
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
		fmt.Sprintf("### Performance Comparison (OCaml): `%s` -> `%s`", rep.From, rep.To),
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
			if m.From != nil {
				if t.From == nil {
					t.From = new(float64)
				}
				*t.From += *m.From
			}
			if m.To != nil {
				if t.To == nil {
					t.To = new(float64)
				}
				*t.To += *m.To
			}
		}
		lines = append(lines, row)
	}
	for _, c := range cols {
		t := totals[c.key]
		t.DeltaPct = deltaPct(t.From, t.To)
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
