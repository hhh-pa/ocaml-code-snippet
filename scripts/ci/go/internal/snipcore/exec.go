package snipcore

import (
	"bytes"
	"os"
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

// Run executes argv with stdin bytes on stdin, capturing stdout and
// stderr, killing the whole process group after timeoutSecs.
func Run(argv []string, stdin []byte, timeoutSecs int) (stdout, stderr []byte, err error) {
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
	runErr := cmd.Run()
	if !timer.Stop() {
		return nil, nil, &TimeoutError{Seconds: timeoutSecs}
	}
	return outBuf.Bytes(), errBuf.Bytes(), runErr
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
