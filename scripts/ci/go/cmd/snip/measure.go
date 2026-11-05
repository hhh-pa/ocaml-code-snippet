package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/measure"
	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/snipcore"
)

const measureUsage = `Load-independent performance measurement for OCaml snippets.

Compiles every src/**/*.ml script to a native binary using ocamlopt -O2,
runs it against the shared test cases under Valgrind Callgrind, and records
simulated instruction reads (Ir) and execution times, plus (under the new
flags) Pin instruction counts from an inscount tool run.

Usage:
  snip measure [--resources DIR] [--repeat N] [--jobs N] [--timeout S]
               [--only SUBSTR] [--shard K/N] [--output JSON]
               [--bmf-output JSON] [--summary-file MD]
               [--pin] [--pin-bin BIN] [--pin-tool SO] [--pin-repeat N]
               [--pin-timeout S]

  snip measure --merge shard-*.json [--output JSON] [--bmf-output JSON]
               [--summary-file MD]

  snip measure --self-test
`

type measureTask struct {
	label    string
	stem     string
	script   string
	caseName string
	inp      string
	outp     string
}

func runMeasure(argv []string) int {
	root := snipcore.RepoRoot()
	resources := filepath.Join(root, "tests", "resources")
	jobs := 2
	repeat := 1
	timeout := 300
	pinRepeat := measure.PinRepeatDefault
	pinTimeout := 600
	var only, shard, output, bmfOutput, summaryFile, mergeFilesStr, pinBin, pinTool string
	var selfTest, pin bool

	// Parse flags manually or with flagSpec; note that --merge can take multiple files.
	// We extract --merge and --self-test first if present.
	var remaining []string
	var mergePaths []string
	isMerge := false

	for i := 0; i < len(argv); i++ {
		arg := argv[i]
		if arg == "--self-test" {
			selfTest = true
		} else if arg == "--merge" {
			isMerge = true
			for i+1 < len(argv) && !strings.HasPrefix(argv[i+1], "-") {
				i++
				mergePaths = append(mergePaths, argv[i])
			}
		} else {
			remaining = append(remaining, arg)
		}
	}

	_, err := parseFlags("measure", remaining, flagSpec{
		Strs: map[string]*string{
			"resources":    &resources,
			"only":         &only,
			"shard":        &shard,
			"output":       &output,
			"bmf-output":   &bmfOutput,
			"summary-file": &summaryFile,
			"merge":        &mergeFilesStr,
			"pin-bin":      &pinBin,
			"pin-tool":     &pinTool,
		},
		Ints: map[string]*int{
			"jobs":        &jobs,
			"repeat":      &repeat,
			"timeout":     &timeout,
			"pin-repeat":  &pinRepeat,
			"pin-timeout": &pinTimeout,
		},
		Bools: map[string]*bool{
			"self-test": &selfTest,
			"pin":       &pin,
		},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(measureUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}

	if pinRepeat < 1 {
		fmt.Fprintln(os.Stderr, "snip measure: --pin-repeat must be >= 1")
		return 2
	}
	if pinTimeout < 1 {
		fmt.Fprintln(os.Stderr, "snip measure: --pin-timeout must be >= 1")
		return 2
	}

	if selfTest {
		return runSelfTest()
	}

	if isMerge || mergeFilesStr != "" {
		if mergeFilesStr != "" {
			mergePaths = append(mergePaths, strings.Fields(mergeFilesStr)...)
		}
		rep, bmf, err := measure.MergeReports(mergePaths)
		if err != nil {
			fmt.Fprintf(os.Stderr, "snip measure merge: %v\n", err)
			return 1
		}
		if output != "" {
			data, _ := json.MarshalIndent(rep, "", "  ")
			if err := os.WriteFile(output, data, 0o666); err != nil {
				fmt.Fprintf(os.Stderr, "write %s: %v\n", output, err)
				return 1
			}
		}
		if bmfOutput != "" {
			data, _ := json.MarshalIndent(bmf, "", "  ")
			if err := os.WriteFile(bmfOutput, data, 0o666); err != nil {
				fmt.Fprintf(os.Stderr, "write %s: %v\n", bmfOutput, err)
				return 1
			}
		}
		if summaryFile != "" {
			if err := measure.AppendSummary(summaryFile, rep, bmf); err != nil {
				fmt.Fprintf(os.Stderr, "append summary: %v\n", err)
			}
		}
		fmt.Printf("Merged %d reports: %d cases, %d benchmarks.\n", len(mergePaths), len(rep.Results), len(bmf))
		return 0
	}

	_, valgrindErr := exec.LookPath("valgrind")
	hasValgrind := valgrindErr == nil
	if !hasValgrind {
		fmt.Fprintln(os.Stderr, "warning: valgrind not found in PATH; running binary directly without Callgrind simulation")
	}

	usePin := pin && measure.PinUsable(pinBin, pinTool)
	if pin && !usePin {
		fmt.Fprintln(os.Stderr, "warning: pin unavailable (need --pin-bin launcher and --pin-tool inscount .so); pin_instructions will be null")
	}
	var pinVersion *string
	if usePin {
		pinVersion = measure.PinVersion(pinBin)
	}
	effectivePin := 0
	if usePin {
		effectivePin = pinRepeat
	}

	_, ocamloptErr := exec.LookPath("ocamlopt")
	hasOcamlopt := ocamloptErr == nil

	found, err := snipcore.Discover(root, resources)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}

	var tasks []measureTask
	for _, entry := range found {
		label := entry.Label
		if only != "" && !strings.Contains(label, only) && !strings.Contains(entry.Script, only) {
			continue
		}
		if len(entry.Cases) == 0 {
			continue
		}
		for _, c := range entry.Cases {
			tasks = append(tasks, measureTask{
				label:    label,
				stem:     entry.Stem,
				script:   entry.Script,
				caseName: c.Name,
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

	// Shard by snippet label (cohesive sharding)
	if shard != "" {
		k, n, err := snipcore.ParseMeasureShard(shard)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			return 1
		}
		groups := map[string][]measureTask{}
		for _, t := range tasks {
			groups[t.label] = append(groups[t.label], t)
		}
		var groupNames []string
		for name := range groups {
			groupNames = append(groupNames, name)
		}
		sort.Strings(groupNames)

		keep := map[string]bool{}
		for i, name := range groupNames {
			if i%n == k {
				keep[name] = true
			}
		}
		var sharded []measureTask
		for _, t := range tasks {
			if keep[t.label] {
				sharded = append(sharded, t)
			}
		}
		tasks = sharded
	}

	totalTasks := len(tasks)
	benchSet := map[string]bool{}
	neededScriptsSet := map[string]bool{}
	for _, t := range tasks {
		benchSet[t.label] = true
		neededScriptsSet[t.script] = true
	}
	var neededScripts []string
	for s := range neededScriptsSet {
		neededScripts = append(neededScripts, s)
	}
	sort.Strings(neededScripts)

	fmt.Printf("Measuring %d cases (%d benchmarks, %d snippets)...\n", totalTasks, len(benchSet), len(neededScripts))

	// Compile snippets if ocamlopt is available
	buildBase := filepath.Join(root, "_build")
	srcDir := filepath.Join(root, "src")
	binaries := map[string]string{}
	compileFailures := map[string]string{}

	if hasOcamlopt {
		fmt.Printf("Compiling %d snippets with ocamlopt -O2...\n", len(neededScripts))
		sem := make(chan struct{}, jobs)
		var wg sync.WaitGroup
		var mu sync.Mutex

		for _, s := range neededScripts {
			wg.Add(1)
			go func(script string) {
				defer wg.Done()
				sem <- struct{}{}
				defer func() { <-sem }()

				rel, _ := filepath.Rel(srcDir, script)
				relStem := strings.TrimSuffix(rel, ".ml")
				binPath := filepath.Join(buildBase, "bin", relStem)
				objDir := filepath.Join(buildBase, "obj", relStem)

				mu.Lock()
				binaries[script] = binPath
				mu.Unlock()

				if fileExists(binPath) {
					return
				}
				_ = os.MkdirAll(filepath.Dir(binPath), 0o755)
				_ = os.MkdirAll(objDir, 0o755)

				absBin, _ := filepath.Abs(binPath)
				absScript, _ := filepath.Abs(script)
				cmd := []string{"ocamlopt", "-O2", "-o", absBin, absScript}
				_, stderr, runErr := snipcore.RunCwd(cmd, nil, objDir, 0)
				if runErr != nil {
					mu.Lock()
					compileFailures[script] = fmt.Sprintf("compile error: %v (%s)", runErr, string(stderr))
					mu.Unlock()
				}
			}(s)
		}
		wg.Wait()
	}

	var results []measure.CaseResult
	var failures []string
	done := 0

	sem := make(chan struct{}, jobs)
	var wg sync.WaitGroup
	var mu sync.Mutex

	for _, t := range tasks {
		if failErr, failedComp := compileFailures[t.script]; failedComp {
			mu.Lock()
			done++
			fmt.Printf("[%d/%d] FAIL %s/%s: %s\n", done, totalTasks, t.label, t.caseName, failErr)
			failures = append(failures, fmt.Sprintf("%s/%s: %s", t.label, t.caseName, failErr))
			mu.Unlock()
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
		go func(tk measureTask, runCmd []string) {
			defer wg.Done()
			sem <- struct{}{}
			defer func() { <-sem }()

			summary, measErr := measure.MeasureCaseCallgrind(hasValgrind, runCmd, tk.inp, tk.outp, repeat, timeout)
			var pinSkipped *string
			if measErr == nil && effectivePin > 0 {
				var pinSummary *measure.NumberSummary
				pinSummary, pinSkipped, measErr = measure.MeasureCasePin(
					tk.label, tk.caseName, runCmd, tk.inp, tk.outp,
					pinBin, pinTool, effectivePin, pinTimeout)
				if measErr == nil && pinSummary != nil {
					summary.PinInstructions = pinSummary
				}
			}

			mu.Lock()
			done++
			if measErr == nil {
				insnStr := "n/a"
				if summary.Instructions != nil {
					insnStr = fmt.Sprintf("%s Ir", formatInt(int64(summary.Instructions.Median)))
				}
				pinInfo := ""
				if summary.PinInstructions != nil {
					pinInfo = fmt.Sprintf("; %s Pin", formatInt(int64(summary.PinInstructions.Median)))
				} else if pinSkipped != nil {
					pinInfo = fmt.Sprintf(" (pin skipped: %s)", *pinSkipped)
				}
				fmt.Printf("[%d/%d] %s/%s: %s%s\n", done, totalTasks, tk.label, tk.caseName, insnStr, pinInfo)
				relScript, _ := filepath.Rel(root, tk.script)
				results = append(results, measure.CaseResult{
					Label:      tk.label,
					Stem:       tk.stem,
					Case:       tk.caseName,
					Script:     filepath.ToSlash(relScript),
					Summary:    summary,
					PinSkipped: pinSkipped,
				})
			} else {
				fmt.Printf("[%d/%d] FAIL %s/%s: %v\n", done, totalTasks, tk.label, tk.caseName, measErr)
				failures = append(failures, fmt.Sprintf("%s/%s: %v", tk.label, tk.caseName, measErr))
			}
			mu.Unlock()
		}(t, cmd)
	}
	wg.Wait()

	sort.Slice(results, func(i, j int) bool {
		if results[i].Label != results[j].Label {
			return results[i].Label < results[j].Label
		}
		return results[i].Case < results[j].Case
	})

	rep := &measure.Report{
		ToolVersion: measure.ToolVersion,
		Platform:    runtime.GOOS + "/" + runtime.GOARCH,
		Date:        time.Now().UTC().Format(time.RFC3339),
		HasValgrind: hasValgrind,
		PinUsed:     usePin,
		PinVersion:  pinVersion,
		Results:     results,
		Failures:    failures,
	}
	bmf := measure.BuildBMF(results)

	if output != "" {
		data, _ := json.MarshalIndent(rep, "", "  ")
		_ = os.WriteFile(output, data, 0o666)
	}
	if bmfOutput != "" {
		data, _ := json.MarshalIndent(bmf, "", "  ")
		_ = os.WriteFile(bmfOutput, data, 0o666)
	}
	if summaryFile != "" {
		_ = measure.AppendSummary(summaryFile, rep, bmf)
	}

	if len(failures) > 0 {
		return 1
	}
	return 0
}

func formatInt(n int64) string {
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

func runSelfTest() int {
	fmt.Println("Running self-test for snip measure...")

	sampleCg := `
events: Ir
summary: 987654321
totals: 987654321
`
	parsed, err := measure.ParseCallgrindOutput(sampleCg)
	if err != nil || parsed != 987654321 {
		fmt.Fprintf(os.Stderr, "ParseCallgrindOutput: expected 987654321, got %d, err: %v\n", parsed, err)
		return 1
	}

	if n, err := measure.ParsePinCount("Count 846018869\n"); err != nil || n != 846018869 {
		fmt.Fprintf(os.Stderr, "ParsePinCount: expected 846018869, got %d, err: %v\n", n, err)
		return 1
	}
	if n, err := measure.ParsePinCount("Pin 3.31\nCount 1\nCount 42\n"); err != nil || n != 42 {
		fmt.Fprintf(os.Stderr, "ParsePinCount: later Count must win, got %d, err: %v\n", n, err)
		return 1
	}
	for _, bad := range []string{"", "no counts here\n", "Count abc\n", "Count\n", "Count 1 2\n"} {
		if n, err := measure.ParsePinCount(bad); err == nil {
			fmt.Fprintf(os.Stderr, "ParsePinCount: must reject %q, got %d\n", bad, n)
			return 1
		}
	}

	mockResults := []measure.CaseResult{
		{
			Label: "sort/merge_sort",
			Case:  "tc0",
			Summary: &measure.CaseSummary{
				Instructions:    &measure.NumberSummary{Median: 1000},
				PinInstructions: &measure.NumberSummary{Median: 5000},
				CpuS:            &measure.NumberSummary{Median: 0.05},
			},
		},
		{
			Label: "sort/merge_sort",
			Case:  "tc1",
			Summary: &measure.CaseSummary{
				Instructions:    &measure.NumberSummary{Median: 2000},
				PinInstructions: &measure.NumberSummary{Median: 7000},
				CpuS:            &measure.NumberSummary{Median: 0.05},
			},
		},
	}
	bmf := measure.BuildBMF(mockResults)
	m := bmf["sort/merge_sort"]
	if m.Instructions == nil || m.Instructions.Value.(int64) != 3000 {
		fmt.Fprintf(os.Stderr, "BMF instructions: expected 3000, got %v\n", m.Instructions)
		return 1
	}
	if m.PinInstructions == nil || m.PinInstructions.Value.(int64) != 12000 {
		fmt.Fprintf(os.Stderr, "BMF pin_instructions: expected 12000, got %v\n", m.PinInstructions)
		return 1
	}
	if m.CpuTime == nil || math.Abs(m.CpuTime.Value.(float64)-0.10) > 1e-6 {
		fmt.Fprintf(os.Stderr, "BMF cpu_time: expected 0.10, got %v\n", m.CpuTime)
		return 1
	}

	fmt.Println("self-test passed!")
	return 0
}
