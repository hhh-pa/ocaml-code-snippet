//go:build !windows

package snipcore

import (
	"os/exec"
	"syscall"
)

func setProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
}

// KillProcessGroup kills the whole group of a timed-out child.
func KillProcessGroup(pid int) {
	_ = syscall.Kill(-pid, syscall.SIGKILL)
}
