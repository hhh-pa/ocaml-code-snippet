package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/snipcore"
)

const testUsage = `Self-contained runner and compiler for the OCaml snippet repo.

Provides separate compilation and test execution phases with granular
per-snippet compilation timing, compiler warning tracking, per-test
execution timing, and comprehensive summaries.

Each test execution records wall time plus wait4 rusage (CPU time,
peak RSS) from that single run, so no second execution is needed to
report resource usage. --stats-output dumps per-case stats as JSON;
--summary-file appends a per-snippet Markdown table. --shard K/N keeps
whole snippets (cohesive, 0-based), the same split snip measure uses.

Usage: snip test [--resources DIR] [--jobs N] [--timeout S] [--only SUBSTR]
                 [--shard K/N] [--compile-only] [--test-only] [--no-compile]
                 [--stats-output JSON] [--summary-file MD]
`

type testTask struct {
	label    string
	caseName string
	script   string
	inp      string
	outp     string
}

type compileResult struct {
	script   string
	label    string
	elapsed  float64
	warnings []string
	err      string
}

type testResult struct {
	label    string
	caseName string
	elapsed  float64
	err      string
}

// testCaseStats is one executed case plus its resource usage. CpuS
// and MaxRSSKb come from wait4 rusage of the test process itself;
// failed-before-run cases (compile errors, unreadable inputs) carry
// zeros with Passed false.
type testCaseStats struct {
	Label    string  `json:"label"`
	Case     string  `json:"case"`
	Passed   bool    `json:"passed"`
	WallS    float64 `json:"wall_s"`
	CpuS     float64 `json:"cpu_s"`
	MaxRSSKb int64   `json:"maxrss_kb"`
}

func runTest(argv []string) int {
	root := snipcore.RepoRoot()
	resources := filepath.Join(root, "tests", "resources")
	jobs := 4
	timeout := 120
	var only, shard, statsOutput, summaryFile string
	var compileOnly, testOnly, noCompile bool

	_, err := parseFlags("test", argv, flagSpec{
		Strs: map[string]*string{
			"resources":    &resources,
			"only":         &only,
			"shard":        &shard,
			"stats-output": &statsOutput,
			"summary-file": &summaryFile,
		},
		Ints: map[string]*int{
			"jobs":    &jobs,
			"timeout": &timeout,
		},
		Bools: map[string]*bool{
			"compile-only": &compileOnly,
			"test-only":    &testOnly,
			"no-compile":   &noCompile,
		},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(testUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}

	shardIndex, shardTotal := 0, 1
	if shard != "" {
		var shardErr error
		shardIndex, shardTotal, shardErr = snipcore.ParseShard(shard)
		if shardErr != nil {
			fmt.Fprintln(os.Stderr, shardErr)
			return 1
		}
	}

	_, ocamloptErr := exec.LookPath("ocamlopt")
	hasOcamlopt := (ocamloptErr == nil) && !noCompile
	_, ocamlErr := exec.LookPath("ocaml")
	hasOcaml := ocamlErr == nil

	if !hasOcamlopt && !hasOcaml {
		fmt.Fprintln(os.Stderr, "warning: neither ocamlopt nor ocaml found in PATH")
	}

	found, err := snipcore.Discover(root, resources)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}

	var tasks []testTask
	var missing []string
	snippetLabels := map[string]string{}

	for _, entry := range found {
		label := entry.Label
		if only != "" && !strings.Contains(label, only) && !strings.Contains(entry.Script, only) {
			continue
		}
		snippetLabels[entry.Script] = label
		if len(entry.Cases) == 0 {
			missing = append(missing, label)
			continue
		}
		for _, c := range entry.Cases {
			tasks = append(tasks, testTask{
				label:    label,
				caseName: c.Name,
				script:   entry.Script,
				inp:      c.Input,
				outp:     c.Output,
			})
		}
	}

	sort.Slice(tasks, func(i, j int) bool {
		if tasks[i].label != tasks[j].label {
			return tasks[i].label < tasks[j].label
		}
		return tasks[i].caseName < tasks[j].caseName
	})

	if shard != "" {
		tasks = snipcore.FilterShard(tasks, func(t testTask) string { return t.label }, shardIndex, shardTotal)
	}

	totalCases := len(tasks)
	neededScriptsSet := map[string]bool{}
	for _, t := range tasks {
		neededScriptsSet[t.script] = true
	}
	var neededScripts []string
	for s := range neededScriptsSet {
		neededScripts = append(neededScripts, s)
	}
	sort.Strings(neededScripts)

	shardStr := ""
	if shard != "" {
		shardStr = fmt.Sprintf(" [shard %d/%d]", shardIndex, shardTotal)
	}
	fmt.Printf("OCaml suite%s: %d test cases across %d snippets\n", shardStr, totalCases, len(neededScripts))

	buildBase := filepath.Join(root, "_build")
	binaries := map[string]string{}
	compileFailures := map[string]string{}
	compileTimings := map[string]float64{}
	compileWarnings := map[string][]string{}

	srcDir := filepath.Join(root, "src")
	for _, s := range neededScripts {
		rel, _ := filepath.Rel(srcDir, s)
		relStem := strings.TrimSuffix(rel, ".ml")
		binaries[s] = filepath.Join(buildBase, "bin", relStem)
	}

	// =========================================================================
	// PHASE 1: COMPILATION
	// =========================================================================
	if !testOnly {
		if hasOcamlopt {
			fmt.Printf("\n=== Compilation Phase (ocamlopt -O2, jobs=%d) ===\n", jobs)
			tCompStart := time.Now()

			sem := make(chan struct{}, jobs)
			var wg sync.WaitGroup
			var mu sync.Mutex
			doneComp := 0

			for _, s := range neededScripts {
				wg.Add(1)
				go func(script string) {
					defer wg.Done()
					sem <- struct{}{}
					defer func() { <-sem }()

					rel, _ := filepath.Rel(srcDir, script)
					relStem := strings.TrimSuffix(rel, ".ml")
					binPath := binaries[script]
					objDir := filepath.Join(buildBase, "obj", relStem)

					_ = os.MkdirAll(filepath.Dir(binPath), 0o755)
					_ = os.MkdirAll(objDir, 0o755)

					absBin, _ := filepath.Abs(binPath)
					absScript, _ := filepath.Abs(script)
					cmd := []string{"ocamlopt", "-O2", "-o", absBin, absScript}

					t0 := time.Now()
					_, stderr, runErr := snipcore.RunCwd(cmd, nil, objDir, 0)
					elapsed := time.Since(t0).Seconds()

					stderrText := strings.TrimSpace(string(stderr))
					var warns []string
					if strings.Contains(stderrText, "Warning") || strings.Contains(stderrText, "warning") {
						for _, line := range strings.Split(stderrText, "\n") {
							if strings.Contains(line, "Warning") || strings.Contains(line, "warning") {
								warns = append(warns, strings.TrimSpace(line))
							}
						}
					}

					var errStr string
					if runErr != nil {
						errStr = fmt.Sprintf("compile error: %v\n%s", runErr, stderrText)
					}

					mu.Lock()
					doneComp++
					label := snippetLabels[script]
					if label == "" {
						label = script
					}
					compileTimings[script] = elapsed
					if len(warns) > 0 {
						compileWarnings[script] = warns
					}
					if errStr != "" {
						compileFailures[script] = errStr
						fmt.Printf("FAIL COMPILE %s [%.3fs] [%d/%d]:\n%s\n", label, elapsed, doneComp, len(neededScripts), errStr)
					} else {
						warnTag := ""
						if len(warns) > 0 {
							warnTag = fmt.Sprintf(" (%d warnings)", len(warns))
						}
						fmt.Printf("COMPILED     %s [%.3fs]%s [%d/%d]\n", label, elapsed, warnTag, doneComp, len(neededScripts))
						for _, w := range warns {
							fmt.Printf("  warning: %s\n", w)
						}
					}
					mu.Unlock()
				}(s)
			}
			wg.Wait()

			tCompWall := time.Since(tCompStart).Seconds()
			var sumCompTime float64
			for _, t := range compileTimings {
				sumCompTime += t
			}
			avgComp := 0.0
			if len(neededScripts) > 0 {
				avgComp = sumCompTime / float64(len(neededScripts))
			}

			fmt.Println("\n=== Compilation Summary ===")
			fmt.Printf("Total snippets : %d\n", len(neededScripts))
			fmt.Printf("Compiled       : %d in %.2fs wall time (sum %.2fs, avg %.3fs/snippet)\n",
				len(neededScripts)-len(compileFailures), tCompWall, sumCompTime, avgComp)
			fmt.Printf("Compile errors : %d\n", len(compileFailures))
			fmt.Printf("Snippets w/warn: %d\n", len(compileWarnings))
			fmt.Println("===========================")

			if len(compileFailures) > 0 && compileOnly {
				return 1
			}
		} else if compileOnly {
			fmt.Fprintln(os.Stderr, "warning: ocamlopt not found; skipping native compilation")
			return 0
		}
	}

	if compileOnly {
		if len(compileFailures) > 0 {
			return 1
		}
		return 0
	}

	// =========================================================================
	// PHASE 2: TEST EXECUTION
	// =========================================================================
	fmt.Printf("\n=== Test Execution Phase (jobs=%d, timeout=%ds) ===\n", jobs, timeout)
	tTestStart := time.Now()

	var failures []string
	passed := 0
	done := 0
	var testTimings []float64
	var caseStats []testCaseStats

	sem := make(chan struct{}, jobs)
	var wg sync.WaitGroup
	var mu sync.Mutex

	for _, t := range tasks {
		if failErr, failedComp := compileFailures[t.script]; failedComp {
			failures = append(failures, fmt.Sprintf("%s/%s: %s", t.label, t.caseName, failErr))
			fmt.Printf("FAIL %s/%s: %s\n", t.label, t.caseName, failErr)
			caseStats = append(caseStats, testCaseStats{Label: t.label, Case: t.caseName})
			done++
			continue
		}

		var cmd []string
		binPath := binaries[t.script]
		if hasOcamlopt && fileExists(binPath) {
			cmd = []string{binPath}
		} else {
			cmd = []string{"ocaml", t.script}
		}

		wg.Add(1)
		go func(tk testTask, runCmd []string) {
			defer wg.Done()
			sem <- struct{}{}
			defer func() { <-sem }()

			inpBytes, readErr := os.ReadFile(tk.inp)
			if readErr != nil {
				mu.Lock()
				done++
				failures = append(failures, fmt.Sprintf("%s/%s: read input: %v", tk.label, tk.caseName, readErr))
				fmt.Printf("FAIL %s/%s: read input: %v\n", tk.label, tk.caseName, readErr)
				caseStats = append(caseStats, testCaseStats{Label: tk.label, Case: tk.caseName})
				mu.Unlock()
				return
			}

			outpBytes, readErr := os.ReadFile(tk.outp)
			if readErr != nil {
				mu.Lock()
				done++
				failures = append(failures, fmt.Sprintf("%s/%s: read output: %v", tk.label, tk.caseName, readErr))
				fmt.Printf("FAIL %s/%s: read output: %v\n", tk.label, tk.caseName, readErr)
				caseStats = append(caseStats, testCaseStats{Label: tk.label, Case: tk.caseName})
				mu.Unlock()
				return
			}
			expected := snipcore.Normalize(outpBytes)

			stdout, stderr, procStat, runErr := snipcore.RunStats(runCmd, inpBytes, timeout)
			elapsed := procStat.WallS

			var caseErr string
			if runErr != nil {
				if _, ok := runErr.(*snipcore.TimeoutError); ok {
					caseErr = fmt.Sprintf("TIMEOUT (>%ds)", timeout)
				} else {
					var exitErr *exec.ExitError
					if errors.As(runErr, &exitErr) {
						tail := string(bytes.TrimSpace(stderr))
						caseErr = fmt.Sprintf("exit %d: %s", exitErr.ExitCode(), tail)
					} else {
						caseErr = fmt.Sprintf("run error: %v", runErr)
					}
				}
			} else {
				normOut := snipcore.Normalize(stdout)
				if !bytes.Equal(normOut, expected) {
					got := string(normOut)
					want := string(expected)
					if len(got) > 200 {
						got = got[:200]
					}
					if len(want) > 200 {
						want = want[:200]
					}
					caseErr = fmt.Sprintf("output mismatch: expected %q got %q", want, got)
				}
			}

			mu.Lock()
			done++
			testTimings = append(testTimings, elapsed)
			caseStats = append(caseStats, testCaseStats{
				Label:    tk.label,
				Case:     tk.caseName,
				Passed:   caseErr == "",
				WallS:    elapsed,
				CpuS:     procStat.CpuS,
				MaxRSSKb: procStat.MaxRSSKb,
			})
			if caseErr == "" {
				passed++
				fmt.Printf("PASS %s/%s in %.3fs [%d/%d]\n", tk.label, tk.caseName, elapsed, done, totalCases)
			} else {
				failures = append(failures, fmt.Sprintf("%s/%s: %s", tk.label, tk.caseName, caseErr))
				fmt.Printf("FAIL %s/%s in %.3fs [%d/%d]: %s\n", tk.label, tk.caseName, elapsed, done, totalCases, caseErr)
			}
			mu.Unlock()
		}(t, cmd)
	}
	wg.Wait()

	tTestWall := time.Since(tTestStart).Seconds()
	var sumTestTime float64
	for _, t := range testTimings {
		sumTestTime += t
	}
	avgTest := 0.0
	if len(testTimings) > 0 {
		avgTest = sumTestTime / float64(len(testTimings))
	}

	for _, label := range missing {
		failures = append(failures, fmt.Sprintf("%s: no resources found", label))
	}

	sort.Slice(caseStats, func(i, j int) bool {
		if caseStats[i].Label != caseStats[j].Label {
			return caseStats[i].Label < caseStats[j].Label
		}
		return caseStats[i].Case < caseStats[j].Case
	})
	if statsOutput != "" {
		if err := writeTestStatsJSON(statsOutput, caseStats); err != nil {
			fmt.Fprintf(os.Stderr, "write %s: %v\n", statsOutput, err)
		}
	}
	if summaryFile != "" {
		if err := appendTestStatsSummary(summaryFile, shardStr, caseStats); err != nil {
			fmt.Fprintf(os.Stderr, "append summary: %v\n", err)
		}
	}

	var sumCPU float64
	var peakRSS int64
	var peakCase string
	for _, s := range caseStats {
		sumCPU += s.CpuS
		if s.MaxRSSKb > peakRSS {
			peakRSS = s.MaxRSSKb
			peakCase = s.Label + "/" + s.Case
		}
	}
	avgCPU := 0.0
	if len(caseStats) > 0 {
		avgCPU = sumCPU / float64(len(caseStats))
	}

	fmt.Println("\n=== Test Execution Summary ===")
	fmt.Printf("Total test cases : %d\n", totalCases)
	fmt.Printf("Passed           : %d in %.2fs wall time (sum %.2fs, avg %.4fs/test)\n",
		passed, tTestWall, sumTestTime, avgTest)
	fmt.Printf("Failed           : %d\n", len(failures))
	fmt.Printf("Missing resources: %d\n", len(missing))
	fmt.Printf("Total CPU time   : %.2fs (avg %.4fs/test)\n", sumCPU, avgCPU)
	if peakCase != "" {
		fmt.Printf("Peak RSS         : %d KB (%s)\n", peakRSS, peakCase)
	}
	printTopCases(caseStats)

	if len(failures) > 0 {
		return 1
	}
	return 0
}

// printTopCases lists the slowest and hungriest executed cases.
func printTopCases(stats []testCaseStats) {
	byCPU := make([]testCaseStats, 0, len(stats))
	byRSS := make([]testCaseStats, 0, len(stats))
	for _, s := range stats {
		if s.CpuS > 0 {
			byCPU = append(byCPU, s)
		}
		if s.MaxRSSKb > 0 {
			byRSS = append(byRSS, s)
		}
	}
	sort.Slice(byCPU, func(i, j int) bool { return byCPU[i].CpuS > byCPU[j].CpuS })
	sort.Slice(byRSS, func(i, j int) bool { return byRSS[i].MaxRSSKb > byRSS[j].MaxRSSKb })
	if len(byCPU) > 0 {
		fmt.Println("\nSlowest cases by CPU:")
		for i, s := range byCPU {
			if i >= 10 {
				break
			}
			fmt.Printf("  %.4fs  %s/%s\n", s.CpuS, s.Label, s.Case)
		}
	}
	if len(byRSS) > 0 {
		fmt.Println("\nTop cases by peak RSS:")
		for i, s := range byRSS {
			if i >= 10 {
				break
			}
			fmt.Printf("  %d KB  %s/%s\n", s.MaxRSSKb, s.Label, s.Case)
		}
	}
}

// writeTestStatsJSON dumps per-case stats sorted by label and case.
func writeTestStatsJSON(path string, stats []testCaseStats) error {
	if stats == nil {
		stats = []testCaseStats{}
	}
	data, err := json.MarshalIndent(stats, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(path, data, 0o666)
}

// appendTestStatsSummary appends a per-snippet CPU/RSS Markdown table.
func appendTestStatsSummary(path, shardSuffix string, stats []testCaseStats) error {
	if path == "" {
		return nil
	}
	snippets := map[string][]testCaseStats{}
	for _, s := range stats {
		snippets[s.Label] = append(snippets[s.Label], s)
	}
	var names []string
	for name := range snippets {
		names = append(names, name)
	}
	sort.Strings(names)

	title := "### Test Resource Stats (OCaml)"
	if shardSuffix != "" {
		title += " [" + strings.Trim(shardSuffix, " []") + "]"
	}
	lines := []string{
		title,
		"",
		"| Benchmark | Cases | CPU Time (s) | Max RSS (KB) |",
		"| :--- | :---: | :---: | :---: |",
	}
	var totalCases int
	var totalCPU float64
	var totalPeak int64
	for _, name := range names {
		items := snippets[name]
		var cpu float64
		var peak int64
		for _, s := range items {
			cpu += s.CpuS
			if s.MaxRSSKb > peak {
				peak = s.MaxRSSKb
			}
		}
		totalCases += len(items)
		totalCPU += cpu
		if peak > totalPeak {
			totalPeak = peak
		}
		lines = append(lines, fmt.Sprintf("| `%s` | %d | %.4f | %d |", name, len(items), cpu, peak))
	}
	lines = append(lines, fmt.Sprintf("| **Total (%d snippets)** | %d | **%.4f** | **%d** |",
		len(names), totalCases, totalCPU, totalPeak))
	lines = append(lines, "")

	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o666)
	if err != nil {
		return err
	}
	defer f.Close()
	_, err = f.WriteString(strings.Join(lines, "\n") + "\n")
	return err
}

func fileExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}
