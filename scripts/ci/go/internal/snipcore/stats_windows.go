//go:build windows

package snipcore

import "os"

// procUsage is a stub on Windows: no rusage support, so CPU and
// RSS stay zero and callers fall back to wall-clock time.
func procUsage(ps *os.ProcessState) (cpuS float64, maxRSSKb int64) {
	return 0, 0
}
