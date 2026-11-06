package main

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// fakeOcamlopt installs an ocamlopt stand-in on PATH running body.
func fakeOcamlopt(t *testing.T, body string) {
	t.Helper()
	if runtime.GOOS == "windows" {
		t.Skip("fake ocamlopt needs sh")
	}
	fakebin := filepath.Join(t.TempDir(), "fakebin")
	if err := os.MkdirAll(fakebin, 0o755); err != nil {
		t.Fatal(err)
	}
	script := "#!/bin/sh\n" + body + "\n"
	path := filepath.Join(fakebin, "ocamlopt")
	if err := os.WriteFile(path, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}
	t.Setenv("PATH", fakebin+string(os.PathListSeparator)+os.Getenv("PATH"))
}

func TestPrecompileReporter(t *testing.T) {
	fakeOcamlopt(t, "printf '%s\\n' \"$*\" > \"$SNIP_ARGV\"\ntouch snip_gc_report.cmx")
	dir := t.TempDir()
	argvFile := filepath.Join(dir, "argv.txt")
	t.Setenv("SNIP_ARGV", argvFile)
	reporter := filepath.Join(dir, "snip_gc_report.ml")
	if err := os.WriteFile(reporter, []byte("let () = ()\n"), 0o666); err != nil {
		t.Fatal(err)
	}
	buildBase := filepath.Join(dir, "_build")

	cmx, err := precompileReporter(buildBase, reporter)
	if err != nil {
		t.Fatalf("precompileReporter: %v", err)
	}
	want := filepath.Join(buildBase, "reporter", "snip_gc_report.cmx")
	if cmx != want {
		t.Fatalf("cmx = %q, want %q", cmx, want)
	}
	if _, err := os.Stat(cmx); err != nil {
		t.Fatalf("cmx missing: %v", err)
	}
	copied, err := os.ReadFile(filepath.Join(buildBase, "reporter", "snip_gc_report.ml"))
	if err != nil || string(copied) != "let () = ()\n" {
		t.Fatalf("reporter copy = %q, %v", copied, err)
	}
	argv, err := os.ReadFile(argvFile)
	if err != nil {
		t.Fatalf("read argv: %v", err)
	}
	if strings.TrimSpace(string(argv)) != "-O2 -c snip_gc_report.ml" {
		t.Fatalf("ocamlopt argv = %q", argv)
	}
}

func TestPrecompileReporterRebuildsStale(t *testing.T) {
	// A no-op ocamlopt must fail even with a stale .cmx present:
	// outputs are always rebuilt, never trusted.
	fakeOcamlopt(t, "exit 0")
	dir := t.TempDir()
	reporter := filepath.Join(dir, "snip_gc_report.ml")
	if err := os.WriteFile(reporter, []byte("let () = ()\n"), 0o666); err != nil {
		t.Fatal(err)
	}
	buildBase := filepath.Join(dir, "_build")
	stale := filepath.Join(buildBase, "reporter", "snip_gc_report.cmx")
	if err := os.MkdirAll(filepath.Dir(stale), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(stale, []byte("stale"), 0o666); err != nil {
		t.Fatal(err)
	}
	if _, err := precompileReporter(buildBase, reporter); err == nil {
		t.Fatal("stale .cmx must not be trusted")
	} else if !strings.Contains(err.Error(), "not produced") {
		t.Fatalf("err = %v, want not-produced", err)
	}
}

func TestPrecompileReporterMissingSource(t *testing.T) {
	fakeOcamlopt(t, "exit 0")
	dir := t.TempDir()
	if _, err := precompileReporter(filepath.Join(dir, "_build"), filepath.Join(dir, "missing.ml")); err == nil {
		t.Fatal("missing reporter source must fail")
	}
}
