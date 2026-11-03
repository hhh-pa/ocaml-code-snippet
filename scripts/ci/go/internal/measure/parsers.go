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
