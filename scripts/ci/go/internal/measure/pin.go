package measure

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/snipcore"
)

// PinRepeatDefault runs two Pin counts per case: native counts
// agree to ~0.1%, enough to catch instrument flakiness without
// paying for more slow runs.
const PinRepeatDefault = 2

// PinUsable reports whether Pin count runs can execute: the pin
// launcher resolves and the inscount tool file exists.
func PinUsable(pinBin, tool string) bool {
	if pinBin == "" || tool == "" {
		return false
	}
	if _, err := os.Stat(tool); err != nil {
		return false
	}
	if strings.Contains(pinBin, string(os.PathSeparator)) {
		info, err := os.Stat(pinBin)
		return err == nil && info.Mode()&0o111 != 0
	}
	_, err := exec.LookPath(pinBin)
	return err == nil
}

// PinVersion probes `pin -version` for report provenance, or nil
// when the launcher cannot answer. Compared runs must share one
// Pin version: counts are only comparable within a version.
func PinVersion(pinBin string) *string {
	stdout, _, err := snipcore.Run([]string{pinBin, "-version"}, nil, 30)
	if err != nil {
		return nil
	}
	for _, line := range strings.Split(string(stdout), "\n") {
		if trimmed := strings.TrimSpace(line); trimmed != "" {
			return &trimmed
		}
	}
	return nil
}

// IsPinTimeout reports whether a PinOnce error text is a deadline
// timeout rather than a crash: timeouts mean the input exceeds
// the instrument's budget, so the case degrades to a skip.
func IsPinTimeout(runErr string) bool {
	return strings.Contains(runErr, "TIMEOUT")
}

// PinOnce runs inner once under Intel Pin with the inscount tool
// and returns stdout plus the total instruction count. The OCaml
// harness has no JVM leg (native ocamlopt binaries, or the ocaml
// interpreter fallback), so this is the plain native recipe with
// no JVM signal flags.
func PinOnce(pinBin, tool string, inner []string, input []byte, timeout int) ([]byte, int64, string) {
	outFile, err := tempPinFile()
	if err != nil {
		return nil, 0, "pin output unreadable: " + err.Error()
	}
	defer os.Remove(outFile)
	cmd := []string{pinBin, "-t", tool, "-o", outFile, "--"}
	cmd = append(cmd, inner...)
	stdout, stderr, err := snipcore.Run(cmd, input, timeout)
	var timeoutErr *snipcore.TimeoutError
	if errors.As(err, &timeoutErr) {
		return nil, 0, fmt.Sprintf("TIMEOUT (>%ds)", timeout)
	}
	if err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			tail := string(bytes.TrimSpace(stderr))
			return nil, 0, fmt.Sprintf("exit %d: %s", exitErr.ExitCode(), tail)
		}
		return nil, 0, fmt.Sprintf("execution error: %v (%s)", err, string(stderr))
	}
	raw, err := os.ReadFile(outFile)
	if err != nil {
		return nil, 0, "pin output unreadable: " + err.Error()
	}
	count, err := ParsePinCount(string(raw))
	if err != nil {
		return nil, 0, err.Error()
	}
	return stdout, count, ""
}

func tempPinFile() (string, error) {
	tmp, err := os.CreateTemp("", "snip-pin-*.out")
	if err != nil {
		return "", err
	}
	name := tmp.Name()
	tmp.Close()
	return name, nil
}

// MeasureCasePin runs the Pin inscount phase for one case: pinRepeat
// counts of execCmd under Pin. It returns the count summary, or a
// skip reason when the case exceeded the Pin budget (callgrind data
// stays valid), or an error when the case crashes or mismatches
// under Pin.
func MeasureCasePin(label, caseName string, execCmd []string, inputPath, outputPath string, pinBin, pinTool string, pinRepeat, pinTimeout int) (*NumberSummary, *string, error) {
	expectedBytes, err := os.ReadFile(outputPath)
	if err != nil {
		return nil, nil, fmt.Errorf("read output file: %w", err)
	}
	expected := snipcore.Normalize(expectedBytes)

	inputBytes, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, nil, fmt.Errorf("read input file: %w", err)
	}

	var counts []float64
	for i := 0; i < pinRepeat; i++ {
		stdout, count, runErr := PinOnce(pinBin, pinTool, execCmd, inputBytes, pinTimeout)
		if runErr != "" {
			if IsPinTimeout(runErr) {
				skip := runErr
				fmt.Fprintf(os.Stderr, "warning: %s/%s: pin skipped (%s)\n",
					label, caseName, runErr)
				return nil, &skip, nil
			}
			return nil, nil, fmt.Errorf("pin: %s", runErr)
		}
		if i == 0 && !bytes.Equal(snipcore.Normalize(stdout), expected) {
			got := string(snipcore.Normalize(stdout))
			want := string(expected)
			if len(got) > 100 {
				got = got[:100]
			}
			if len(want) > 100 {
				want = want[:100]
			}
			return nil, nil, fmt.Errorf("pin output mismatch: expected %q got %q", want, got)
		}
		counts = append(counts, float64(count))
	}
	return SummarizeNumbers(counts), nil, nil
}
