//go:build !windows

package snipcore

import (
	"os"
	"runtime"
	"syscall"
)

// procUsage extracts CPU time (user+sys, seconds) and peak RSS
// (kilobytes) from a waited child. Maxrss units are platform
// specific: kilobytes on Linux, bytes on Darwin.
func procUsage(ps *os.ProcessState) (cpuS float64, maxRSSKb int64) {
	if ps == nil {
		return 0, 0
	}
	ru, ok := ps.SysUsage().(*syscall.Rusage)
	if !ok || ru == nil {
		return 0, 0
	}
	cpuS = float64(ru.Utime.Sec) + float64(ru.Utime.Usec)/1e6 +
		float64(ru.Stime.Sec) + float64(ru.Stime.Usec)/1e6
	maxRSSKb = ru.Maxrss
	if runtime.GOOS == "darwin" {
		maxRSSKb /= 1024
	}
	return cpuS, maxRSSKb
}
