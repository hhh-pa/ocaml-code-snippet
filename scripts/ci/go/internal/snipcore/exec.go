package snipcore

import (
	"bytes"
	"os/exec"
	"time"
)

// TimeoutError marks a subprocess that exceeded its deadline.
type TimeoutError struct {
	Seconds int
}

func (e *TimeoutError) Error() string {
	return "timeout"
}

// ProcStats holds per-process resource usage for one child run:
// wall-clock time plus wait4 rusage (CPU time, peak RSS). CpuS
// and MaxRSSKb are zero on timeouts and on platforms without
// rusage support.
type ProcStats struct {
	WallS    float64
	CpuS     float64
	MaxRSSKb int64
}

// Run executes argv with stdin bytes on stdin, capturing stdout and
// stderr, killing the whole process group after timeoutSecs.
func Run(argv []string, stdin []byte, timeoutSecs int) (stdout, stderr []byte, err error) {
	stdout, stderr, _, err = RunStats(argv, stdin, timeoutSecs)
	return stdout, stderr, err
}

// RunStats is Run plus per-process resource usage from wait4
// rusage. Under instrumentation (valgrind, Pin) the CPU and RSS
// figures describe the tool process, tool overhead included.
func RunStats(argv []string, stdin []byte, timeoutSecs int) (stdout, stderr []byte, stats ProcStats, err error) {
	cmd := exec.Command(argv[0], argv[1:]...)
	setProcessGroup(cmd)
	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}
	var outBuf, errBuf bytes.Buffer
	cmd.Stdout = &outBuf
	cmd.Stderr = &errBuf
	timer := time.AfterFunc(time.Duration(timeoutSecs)*time.Second, func() {
		if cmd.Process != nil {
			KillProcessGroup(cmd.Process.Pid)
		}
	})
	t0 := time.Now()
	runErr := cmd.Run()
	stats.WallS = time.Since(t0).Seconds()
	if !timer.Stop() {
		return nil, nil, ProcStats{}, &TimeoutError{Seconds: timeoutSecs}
	}
	stats.CpuS, stats.MaxRSSKb = procUsage(cmd.ProcessState)
	return outBuf.Bytes(), errBuf.Bytes(), stats, runErr
}

// RunCwd executes argv within cwd.
func RunCwd(argv []string, stdin []byte, cwd string, timeoutSecs int) (stdout, stderr []byte, err error) {
	cmd := exec.Command(argv[0], argv[1:]...)
	if cwd != "" {
		cmd.Dir = cwd
	}
	setProcessGroup(cmd)
	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}
	var outBuf, errBuf bytes.Buffer
	cmd.Stdout = &outBuf
	cmd.Stderr = &errBuf
	var timer *time.Timer
	if timeoutSecs > 0 {
		timer = time.AfterFunc(time.Duration(timeoutSecs)*time.Second, func() {
			if cmd.Process != nil {
				KillProcessGroup(cmd.Process.Pid)
			}
		})
	}
	runErr := cmd.Run()
	if timer != nil && !timer.Stop() {
		return nil, nil, &TimeoutError{Seconds: timeoutSecs}
	}
	return outBuf.Bytes(), errBuf.Bytes(), runErr
}
