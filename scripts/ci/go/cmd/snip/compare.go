// Compare two `snip measure` full-report JSONs by snippet label.
//
// `snip compare` is the performance-comparison leg of CI: the sweep
// measures a from-tree and a to-tree per shard, the report job
// merges each side, and compare diffs the merged reports using
// summed-median BMF totals. Correspondence gaps are data, never
// errors: a snippet missing on one side shows n/a there, since
// trees legitimately add and drop snippets across commits.
// Report-only: exit 0 whenever the comparison itself runs; only
// flag/IO errors exit nonzero.
package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"

	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/measure"
)

const compareUsage = `Compare two snip measure full-report JSONs.

Usage: snip compare --from FROM.json --to TO.json [--output F]
                   [--summary-file F]
                   [--from-label L] [--to-label L]

Diffs the reports per snippet label using summed-median BMF totals;
snippets missing on a side show n/a there. --from-label/--to-label
name the sides in the summary header (default "from"/"to").
`

func runCompare(argv []string) int {
	var fromPath, toPath, output, summaryFile, fromLabel, toLabel string
	fromLabel = "from"
	toLabel = "to"
	_, err := parseFlags("compare", argv, flagSpec{
		Strs: map[string]*string{
			"from": &fromPath, "to": &toPath,
			"output": &output, "summary-file": &summaryFile,
			"from-label": &fromLabel, "to-label": &toLabel,
		},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(compareUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	if fromPath == "" || toPath == "" {
		fmt.Fprintln(os.Stderr, "snip compare: --from and --to are required")
		return 2
	}
	fromRep, err := readReport(fromPath)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	toRep, err := readReport(toPath)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	c := measure.CompareReports(&fromRep, &toRep, fromLabel, toLabel)
	if output != "" {
		data, _ := json.MarshalIndent(c, "", "  ")
		if err := os.WriteFile(output, data, 0o666); err != nil {
			fmt.Fprintf(os.Stderr, "write %s: %v\n", output, err)
			return 1
		}
	}
	if summaryFile != "" {
		if err := measure.AppendCompareSummary(summaryFile, c); err != nil {
			fmt.Fprintf(os.Stderr, "append summary: %v\n", err)
		}
	}
	fmt.Printf("compare %s -> %s: %d snippets.\n", fromLabel, toLabel, len(c.Results))
	return 0
}

func readReport(path string) (measure.Report, error) {
	var rep measure.Report
	raw, err := os.ReadFile(path)
	if err != nil {
		return rep, fmt.Errorf("cannot read report %s: %v", path, err)
	}
	if err := json.Unmarshal(raw, &rep); err != nil {
		return rep, fmt.Errorf("cannot read report %s: %v", path, err)
	}
	return rep, nil
}
