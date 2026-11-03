// Command snip replaces the repo's Python and shell tooling with one
// statically built binary:
//
//	snip test     correctness runner and compiler (tests/run_tests.py)
//	snip measure  load-independent perf measurement (tools/measure.py)
//	snip bundle   encrypted resource-bundle builder (tools/build_bundle.sh)
//	snip decrypt  decrypt and extract test resources
//
// Run from the repository root.
package main

import (
	"fmt"
	"os"
)

func main() {
	os.Exit(dispatch(os.Args[1:]))
}

func dispatch(argv []string) int {
	if len(argv) == 0 {
		fmt.Print(usage)
		return 2
	}
	switch argv[0] {
	case "-h", "--help", "help":
		fmt.Print(usage)
		return 0
	case "test":
		return runTest(argv[1:])
	case "measure":
		return runMeasure(argv[1:])
	case "bundle":
		return runBundle(argv[1:])
	case "decrypt":
		return runDecrypt(argv[1:])
	default:
		fmt.Fprintf(os.Stderr, "snip: unknown subcommand %q\n%s", argv[0], usage)
		return 2
	}
}

const usage = `Usage: snip <subcommand> [flags]

Subcommands:
  test      compile and run OCaml snippets against shared test cases
  measure   load-independent performance measurement under Callgrind
  bundle    build or rotate the encrypted test-resource bundle
  decrypt   decrypt and extract the test-resource bundle

Run from the repository root. Each subcommand accepts -h/--help.
`
