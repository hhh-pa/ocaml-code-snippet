package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
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

func TestSnipCompare(t *testing.T) {
	bin := cmdtest.Build(t, ".")
	dir := t.TempDir()
	report := func(pin int64) string {
		p := strconv.FormatInt(pin, 10)
		return `{"tool_version":4,"results":[` +
			`{"label":"s/x","stem":"x","case":"tc0","script":"s",` +
			`"summary":{"runs":0,"pin_instructions":{"min":` + p +
			`,"median":` + p + `,"max":` + p + `}}}]` +
			`,"failures":[]}`
	}
	if err := os.WriteFile(filepath.Join(dir, "from.json"), []byte(report(1000)), 0o666); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "to.json"), []byte(report(1500)), 0o666); err != nil {
		t.Fatal(err)
	}
	out, code := cmdtest.Run(t, bin, dir, "compare",
		"--from", "from.json", "--to", "to.json",
		"--from-label", "f", "--to-label", "t",
		"--output", "c.json", "--summary-file", "c.md")
	if code != 0 {
		t.Fatalf("exit = %d, want 0\n%s", code, out)
	}
	if !strings.Contains(out, "compare f -> t: 1 snippets.") {
		t.Fatalf("output lacks summary:\n%s", out)
	}
	data, err := os.ReadFile(filepath.Join(dir, "c.json"))
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{`"from": "f"`, `"to": "t"`, `"delta_pct": 50`} {
		if !strings.Contains(string(data), want) {
			t.Fatalf("c.json lacks %s:\n%s", want, data)
		}
	}
	md, err := os.ReadFile(filepath.Join(dir, "c.md"))
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(md), "1,000 -> 1,500 (+50.00%)") {
		t.Fatalf("summary lacks delta row:\n%s", md)
	}
}

func TestSnipMeasureCallgrindNeedsValgrind(t *testing.T) {
	bin := cmdtest.Build(t, ".")
	dir := t.TempDir()
	// Empty PATH guarantees no valgrind; the flag must fail loud
	// instead of silently recording direct runs.
	_, stderr, code := cmdtest.RunBothEnv(t, bin, dir,
		[]string{"PATH=" + t.TempDir()}, "measure", "--repeat", "1", "--callgrind")
	if code != 1 {
		t.Fatalf("exit = %d, want 1", code)
	}
	if !strings.Contains(stderr, "--callgrind needs valgrind on PATH") {
		t.Fatalf("stderr lacks loud failure:\n%s", stderr)
	}
}

func TestSnipMeasureCallgrindRecordsIr(t *testing.T) {
	if _, err := exec.LookPath("ocamlopt"); err != nil {
		t.Skip("needs ocamlopt")
	}
	if _, err := exec.LookPath("valgrind"); err != nil {
		t.Skip("needs valgrind")
	}
	bin := cmdtest.Build(t, ".")
	dir := t.TempDir()
	writeFile := func(path, content string) {
		t.Helper()
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), 0o666); err != nil {
			t.Fatal(err)
		}
	}
	writeFile(filepath.Join(dir, "src", "a", "b.ml"),
		"let () = print_string (try input_line stdin with End_of_file -> \"\")\n")
	writeFile(filepath.Join(dir, "res", "a", "b", "test_case_0", "input.txt"), "hi\n")
	writeFile(filepath.Join(dir, "res", "a", "b", "test_case_0", "output.txt"), "hi\n")
	_, _, code := cmdtest.RunBothEnv(t, bin, dir, nil, "measure",
		"--resources", "res", "--repeat", "1", "--callgrind", "--bmf-output", "b.json")
	if code != 0 {
		t.Fatalf("exit = %d, want 0", code)
	}
	data, err := os.ReadFile(filepath.Join(dir, "b.json"))
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(data), `"instructions"`) {
		t.Fatalf("bmf lacks Ir signal:\n%s", data)
	}
}

func TestSnipMeasureMissingResources(t *testing.T) {
	bin := cmdtest.Build(t, ".")
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "src", "a"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "src", "a", "b.ml"), []byte("let () = ()\n"), 0o666); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(dir, "tests", "resources"), 0o755); err != nil {
		t.Fatal(err)
	}
	_, _, code := cmdtest.RunBothEnv(t, bin, dir, nil, "measure",
		"--resources", "tests/resources", "--repeat", "1", "--output", "rep.json")
	if code != 1 {
		t.Fatalf("exit = %d, want 1 (missing resources fail the sweep)", code)
	}
	data, err := os.ReadFile(filepath.Join(dir, "rep.json"))
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(data), "a/b: no resources found") {
		t.Fatalf("report lacks missing failure:\n%s", data)
	}
}

func TestSnipMeasurePinNeedsKit(t *testing.T) {
	bin := cmdtest.Build(t, ".")
	dir := t.TempDir()
	_, stderr, code := cmdtest.RunBothEnv(t, bin, dir, nil, "measure",
		"--repeat", "1", "--pin",
		"--pin-bin", "/nonexistent/pin", "--pin-tool", "/nonexistent/inscount.so")
	if code != 1 {
		t.Fatalf("exit = %d, want 1", code)
	}
	if !strings.Contains(stderr, "--pin needs a usable kit") {
		t.Fatalf("stderr lacks loud failure:\n%s", stderr)
	}
}

// missingFixture writes two resourceless scripts (labels a, b) with
// an empty resources dir: no toolchain runs, so the CLI must fail
// with exactly the missing-resource failures.
func missingFixture(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "src"), 0o755); err != nil {
		t.Fatal(err)
	}
	for _, stem := range []string{"a", "b"} {
		if err := os.WriteFile(filepath.Join(dir, "src", stem+".ml"),
			[]byte("let () = ()\n"), 0o666); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.MkdirAll(filepath.Join(dir, "tests", "resources"), 0o755); err != nil {
		t.Fatal(err)
	}
	return dir
}

func TestSnipMeasureMissingShardScoped(t *testing.T) {
	bin := cmdtest.Build(t, ".")
	dir := missingFixture(t)
	// Labels sort to a, b: each shard must report only its own
	// missing label, never the other shard's.
	for _, tc := range []struct{ shard, want, avoid string }{
		{"0/2", "a: no resources found", "b: no resources found"},
		{"1/2", "b: no resources found", "a: no resources found"},
	} {
		out := "rep-" + strings.ReplaceAll(tc.shard, "/", "-") + ".json"
		_, _, code := cmdtest.RunBothEnv(t, bin, dir, nil, "measure",
			"--resources", "tests/resources", "--repeat", "1",
			"--shard", tc.shard, "--output", out)
		if code != 1 {
			t.Fatalf("shard %s: exit = %d, want 1", tc.shard, code)
		}
		data, err := os.ReadFile(filepath.Join(dir, out))
		if err != nil {
			t.Fatal(err)
		}
		if !strings.Contains(string(data), tc.want) {
			t.Fatalf("shard %s lacks %q:\n%s", tc.shard, tc.want, data)
		}
		if strings.Contains(string(data), tc.avoid) {
			t.Fatalf("shard %s must not report the other shard's label:\n%s", tc.shard, data)
		}
	}
}

func TestSnipTestMissingShardScoped(t *testing.T) {
	bin := cmdtest.Build(t, ".")
	dir := missingFixture(t)
	for _, shard := range []string{"0/2", "1/2"} {
		out, code := cmdtest.Run(t, bin, dir, "test",
			"--resources", "tests/resources", "--shard", shard)
		if code != 1 {
			t.Fatalf("shard %s: exit = %d, want 1", shard, code)
		}
		if !strings.Contains(out, "Missing resources: 1") {
			t.Fatalf("shard %s lacks scoped count:\n%s", shard, out)
		}
	}
}
