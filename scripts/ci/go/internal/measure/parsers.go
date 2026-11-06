package measure

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

var callgrindTotalRe = regexp.MustCompile(`(?m)^(?:totals|summary):\s*(\d+)`)

// ParseCallgrindOutput extracts total simulated instructions (Ir) from Callgrind output text.
func ParseCallgrindOutput(content string) (int64, error) {
	matches := callgrindTotalRe.FindAllStringSubmatch(content, -1)
	if len(matches) > 0 {
		last := matches[len(matches)-1]
		if n, err := strconv.ParseInt(last[1], 10, 64); err == nil {
			return n, nil
		}
	}
	for _, line := range strings.Split(content, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "totals:") || strings.HasPrefix(trimmed, "summary:") {
			parts := strings.Split(trimmed, ":")
			if len(parts) >= 2 {
				valFields := strings.Fields(parts[1])
				if len(valFields) > 0 {
					if n, err := strconv.ParseInt(valFields[0], 10, 64); err == nil {
						return n, nil
					}
				}
			}
		}
	}
	return 0, fmt.Errorf("no callgrind totals/summary found")
}

// AllocMarker prefixes the allocation line the snip_gc_report
// module prints to stderr at program exit.
const AllocMarker = "SNIP_ALLOCATED_BYTES="

// ParseAllocBytes reads total allocated bytes from a snippet run's
// stderr. It reports found=false when the marker is absent (the
// binary was built without the reporter: `snip test` binaries, the
// ocaml-interpreter fallback, or a reporter-less checkout), so
// callers record no allocated_bytes rather than zero. Later valid
// markers win; malformed marker lines are skipped, never fatal:
// stderr is shared with arbitrary snippet output.
func ParseAllocBytes(stderr string) (bytes int64, found bool) {
	for _, line := range strings.Split(stderr, "\n") {
		trimmed := strings.TrimSpace(line)
		if !strings.HasPrefix(trimmed, AllocMarker) {
			continue
		}
		n, err := strconv.ParseInt(strings.TrimSpace(trimmed[len(AllocMarker):]), 10, 64)
		if err != nil || n < 0 {
			continue
		}
		bytes, found = n, true
	}
	return bytes, found
}

// ParsePinCount reads the total from an inscount-style tool
// output file (one "Count N" line). Later Count lines win so
// per-thread dumps still resolve to their trailing total.
func ParsePinCount(text string) (int64, error) {
	found := false
	var total int64
	for _, line := range strings.Split(text, "\n") {
		fields := strings.Fields(line)
		if len(fields) != 2 || fields[0] != "Count" {
			continue
		}
		n, err := strconv.ParseInt(fields[1], 10, 64)
		if err != nil {
			return 0, fmt.Errorf("pin count unreadable: %q", strings.TrimSpace(line))
		}
		total, found = n, true
	}
	if !found {
		return 0, fmt.Errorf("pin count unreadable: no Count line")
	}
	return total, nil
}
