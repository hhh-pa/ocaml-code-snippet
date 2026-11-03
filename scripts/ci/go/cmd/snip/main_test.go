package main

import (
	"strings"
	"testing"

	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/cmdtest"
)

func TestSnipUsage(t *testing.T) {
	bin := cmdtest.Build(t, ".")
	out, code := cmdtest.Run(t, bin, t.TempDir(), "--help")
	if code != 0 {
		t.Fatalf("exit = %d, want 0\n%s", code, out)
	}
	if !strings.Contains(out, "Usage: snip <subcommand>") {
		t.Fatalf("output lacks usage:\n%s", out)
	}
}

func TestSnipSelfTest(t *testing.T) {
	bin := cmdtest.Build(t, ".")
	out, code := cmdtest.Run(t, bin, t.TempDir(), "measure", "--self-test")
	if code != 0 {
		t.Fatalf("exit = %d, want 0\n%s", code, out)
	}
	if !strings.Contains(out, "self-test passed!") {
		t.Fatalf("output lacks pass message:\n%s", out)
	}
}
