// Package snipcore holds the logic shared by the snip subcommands:
// repository discovery, output normalization, subprocess execution,
// and shard selection for the OCaml code snippet repo.
package snipcore

import (
	"os"
)

// RepoRoot returns the repository root, which for the Go tools is the
// process working directory: snip runs from the repo root (CI checks
// out and runs there; local runs do the same).
func RepoRoot() string {
	wd, err := os.Getwd()
	if err != nil {
		return "."
	}
	return wd
}
