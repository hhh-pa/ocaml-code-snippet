package measure

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"time"

	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/snipcore"
)

// MeasureCaseCallgrind runs a single test case under Callgrind or directly,
// repeats repeat times, and computes the CaseSummary.
func MeasureCaseCallgrind(hasValgrind bool, execCmd []string, inputPath, outputPath string, repeat, timeoutSecs int) (*CaseSummary, error) {
	expectedBytes, err := os.ReadFile(outputPath)
	if err != nil {
		return nil, fmt.Errorf("read output file: %w", err)
	}
	expected := snipcore.Normalize(expectedBytes)

	inputBytes, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, fmt.Errorf("read input file: %w", err)
	}

	var insnVals []float64
	var wallVals []float64
	var cpuVals []float64
	var rssVals []float64

	for i := 0; i < repeat; i++ {
		var cgFile string
		var cmd []string

		if hasValgrind {
			tmp, err := os.CreateTemp("", "snip-cg-*.out")
			if err != nil {
				return nil, err
			}
			cgFile = tmp.Name()
			tmp.Close()
			defer os.Remove(cgFile)

			cmd = append([]string{"valgrind", "--tool=callgrind", fmt.Sprintf("--callgrind-out-file=%s", cgFile)}, execCmd...)
		} else {
			cmd = execCmd
		}

		t0 := time.Now()
		stdout, stderr, runErr := snipcore.Run(cmd, inputBytes, timeoutSecs)
		elapsed := time.Since(t0).Seconds()

		if runErr != nil {
			if _, ok := runErr.(*snipcore.TimeoutError); ok {
				return nil, fmt.Errorf("TIMEOUT (>%ds)", timeoutSecs)
			}
			var exitErr *exec.ExitError
			if ok := isExitError(runErr, &exitErr); ok {
				tail := string(bytes.TrimSpace(stderr))
				return nil, fmt.Errorf("exit %d: %s", exitErr.ExitCode(), tail)
			}
			return nil, fmt.Errorf("execution error: %w (%s)", runErr, string(stderr))
		}

		normOut := snipcore.Normalize(stdout)
		if !bytes.Equal(normOut, expected) {
			got := string(normOut)
			want := string(expected)
			if len(got) > 100 {
				got = got[:100]
			}
			if len(want) > 100 {
				want = want[:100]
			}
			return nil, fmt.Errorf("output mismatch: expected %q got %q", want, got)
		}

		if hasValgrind && cgFile != "" {
			cgBytes, err := os.ReadFile(cgFile)
			if err == nil {
				if ir, err := ParseCallgrindOutput(string(cgBytes)); err == nil {
					insnVals = append(insnVals, float64(ir))
				}
			}
		}

		wallVals = append(wallVals, elapsed)
		cpuVals = append(cpuVals, elapsed)
		rssVals = append(rssVals, 0)
	}

	summary := &CaseSummary{
		Instructions: SummarizeNumbers(insnVals),
		WallS:        SummarizeNumbers(wallVals),
		CpuS:         SummarizeNumbers(cpuVals),
		MaxRSSKb:     SummarizeNumbers(rssVals),
		Runs:         repeat,
	}
	return summary, nil
}

func isExitError(err error, target **exec.ExitError) bool {
	if ee, ok := err.(*exec.ExitError); ok {
		*target = ee
		return true
	}
	return false
}
