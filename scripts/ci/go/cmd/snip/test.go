package main

import (
	"bytes"
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

Usage: snip test [--resources DIR] [--jobs N] [--timeout S] [--only SUBSTR]
                 [--shard I/N] [--compile-only] [--test-only] [--no-compile]
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

func runTest(argv []string) int {
	root := snipcore.RepoRoot()
	resources := filepath.Join(root, "tests", "resources")
	jobs := 4
	timeout := 120
	var only, shard string
	var compileOnly, testOnly, noCompile bool

	_, err := parseFlags("test", argv, flagSpec{
		Strs: map[string]*string{
			"resources": &resources,
			"only":      &only,
			"shard":     &shard,
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
		shardIndex, shardTotal, shardErr = snipcore.ParseTestShard(shard)
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

	if shardTotal > 1 {
		var sharded []testTask
		for i, t := range tasks {
			if i%shardTotal == shardIndex {
				sharded = append(sharded, t)
			}
		}
		tasks = sharded
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
	if shardTotal > 1 {
		shardStr = fmt.Sprintf(" [shard %d/%d]", shardIndex+1, shardTotal)
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
			fmt.Println("===========================\n")

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

	sem := make(chan struct{}, jobs)
	var wg sync.WaitGroup
	var mu sync.Mutex

	for _, t := range tasks {
		if failErr, failedComp := compileFailures[t.script]; failedComp {
			failures = append(failures, fmt.Sprintf("%s/%s: %s", t.label, t.caseName, failErr))
			fmt.Printf("FAIL %s/%s: %s\n", t.label, t.caseName, failErr)
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
				mu.Unlock()
				return
			}

			outpBytes, readErr := os.ReadFile(tk.outp)
			if readErr != nil {
				mu.Lock()
				done++
				failures = append(failures, fmt.Sprintf("%s/%s: read output: %v", tk.label, tk.caseName, readErr))
				fmt.Printf("FAIL %s/%s: read output: %v\n", tk.label, tk.caseName, readErr)
				mu.Unlock()
				return
			}
			expected := snipcore.Normalize(outpBytes)

			t0 := time.Now()
			stdout, stderr, runErr := snipcore.Run(runCmd, inpBytes, timeout)
			elapsed := time.Since(t0).Seconds()

			var caseErr string
			if runErr != nil {
				if _, ok := runErr.(*snipcore.TimeoutError); ok {
					caseErr = fmt.Sprintf("TIMEOUT (>%ds)", timeout)
				} else {
					var exitErr *exec.ExitError
					if ok := isExitError(runErr, &exitErr); ok {
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

	fmt.Println("\n=== Test Execution Summary ===")
	fmt.Printf("Total test cases : %d\n", totalCases)
	fmt.Printf("Passed           : %d in %.2fs wall time (sum %.2fs, avg %.4fs/test)\n",
		passed, tTestWall, sumTestTime, avgTest)
	fmt.Printf("Failed           : %d\n", len(failures))
	fmt.Printf("Missing resources: %d\n", len(missing))

	if len(failures) > 0 {
		return 1
	}
	return 0
}

func fileExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}
