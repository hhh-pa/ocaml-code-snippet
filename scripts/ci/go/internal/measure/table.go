package measure

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

// AppendSummary formats and appends the Markdown summary table to path.
func AppendSummary(path string, report *Report, bmf BMF) error {
	if path == "" {
		return nil
	}

	snippets := map[string][]CaseResult{}
	for _, r := range report.Results {
		snippets[r.Label] = append(snippets[r.Label], r)
	}

	var names []string
	for name := range snippets {
		names = append(names, name)
	}
	sort.Strings(names)

	var lines []string
	lines = append(lines, "### Performance Summary (OCaml)")
	lines = append(lines, "")
	lines = append(lines, "| Benchmark | Cases | Instructions (Ir) | Pin Instructions | CPU Time (s) |")
	lines = append(lines, "| :--- | :---: | :---: | :---: | :---: |")

	var totalInsn int64
	var totalPin int64
	var totalCpu float64
	hasInsn := false
	hasPin := false

	for _, name := range names {
		casesCount := len(snippets[name])
		m, ok := bmf[name]
		insnStr := "-"
		if ok && m.Instructions != nil {
			val, _ := toInt64(m.Instructions.Value)
			insnStr = fmtInt(val)
			totalInsn += val
			hasInsn = true
		}
		pinStr := "-"
		if ok && m.PinInstructions != nil {
			val, _ := toInt64(m.PinInstructions.Value)
			pinStr = fmtInt(val)
			totalPin += val
			hasPin = true
		}
		var cpuVal float64
		if ok && m.CpuTime != nil {
			cpuVal, _ = toFloat64(m.CpuTime.Value)
			totalCpu += cpuVal
		}
		lines = append(lines, fmt.Sprintf("| `%s` | %d | %s | %s | %.4f |", name, casesCount, insnStr, pinStr, cpuVal))
	}

	totInsnStr := "-"
	if hasInsn {
		totInsnStr = fmtInt(totalInsn)
	}
	totPinStr := "-"
	if hasPin {
		totPinStr = fmtInt(totalPin)
	}
	lines = append(lines, fmt.Sprintf("| **Total (%d snippets)** | - | **%s** | **%s** | **%.4f** |", len(names), totInsnStr, totPinStr, totalCpu))
	lines = append(lines, "")

	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o666)
	if err != nil {
		return err
	}
	defer f.Close()

	_, err = f.WriteString(strings.Join(lines, "\n") + "\n")
	return err
}

func fmtInt(n int64) string {
	in := fmt.Sprintf("%d", n)
	var out []byte
	l := len(in)
	for i := 0; i < l; i++ {
		if i > 0 && (l-i)%3 == 0 {
			out = append(out, ',')
		}
		out = append(out, in[i])
	}
	return string(out)
}

func toInt64(v any) (int64, bool) {
	switch n := v.(type) {
	case int64:
		return n, true
	case int:
		return int64(n), true
	case float64:
		return int64(n), true
	default:
		return 0, false
	}
}

func toFloat64(v any) (float64, bool) {
	switch n := v.(type) {
	case float64:
		return n, true
	case float32:
		return float64(n), true
	case int:
		return float64(n), true
	case int64:
		return float64(n), true
	default:
		return 0, false
	}
}
