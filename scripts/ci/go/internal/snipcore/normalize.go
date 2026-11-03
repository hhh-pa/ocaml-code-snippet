package snipcore

import (
	"strings"
)

const asciiSpace = " \t\n\r\x0b\x0c"

// Normalize compares program output the way tests/run_tests.py does:
// splits on line breaks with ASCII-whitespace stripping of every line.
func Normalize(content []byte) []byte {
	text := string(content)
	text = strings.ReplaceAll(text, "\r\n", "\n")
	text = strings.ReplaceAll(text, "\r", "\n")
	lines := strings.Split(text, "\n")
	if strings.HasSuffix(text, "\n") {
		lines = lines[:len(lines)-1]
	}
	for i, line := range lines {
		lines[i] = strings.Trim(line, asciiSpace)
	}
	return []byte(strings.Join(lines, "\n"))
}
