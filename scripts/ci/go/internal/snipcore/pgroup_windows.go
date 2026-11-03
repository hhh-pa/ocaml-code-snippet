//go:build windows

package snipcore

import (
	"os/exec"
)

func setProcessGroup(cmd *exec.Cmd) {}

func KillProcessGroup(pid int) {}
