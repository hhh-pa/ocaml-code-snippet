package measure

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"os/exec"

	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/snipcore"
)

// MeasureCaseCallgrind runs a single test case under Callgrind or directly,
// repeats repeat times, and computes the CaseSummary. CpuS and MaxRSSKb
// come from wait4 rusage of each run; under valgrind they describe the
// valgrind process (tool overhead included), on direct runs the snippet.
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

		stdout, stderr, procStat, runErr := snipcore.RunStats(cmd, inputBytes, timeoutSecs)

		if runErr != nil {
			if _, ok := runErr.(*snipcore.TimeoutError); ok {
				return nil, fmt.Errorf("TIMEOUT (>%ds)", timeoutSecs)
			}
			var exitErr *exec.ExitError
			if errors.As(runErr, &exitErr) {
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

		wallVals = append(wallVals, procStat.WallS)
		cpuVals = append(cpuVals, procStat.CpuS)
		rssVals = append(rssVals, float64(procStat.MaxRSSKb))
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
