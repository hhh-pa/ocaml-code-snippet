package snipcore

import (
	"bytes"
	"runtime"
	"strings"
	"testing"
)

func TestNormalize(t *testing.T) {
	input := []byte("  line 1  \r\n  line 2\t\n\nline 3   \r")
	want := []byte("line 1\nline 2\n\nline 3")
	got := Normalize(input)
	if !bytes.Equal(got, want) {
		t.Fatalf("Normalize:\n got: %q\nwant: %q", got, want)
	}
}

func TestParseShard(t *testing.T) {
	idx, tot, err := ParseShard("0/4")
	if err != nil || idx != 0 || tot != 4 {
		t.Fatalf("0/4: got (%d, %d, %v)", idx, tot, err)
	}
	idx, tot, err = ParseShard("3/4")
	if err != nil || idx != 3 || tot != 4 {
		t.Fatalf("3/4: got (%d, %d, %v)", idx, tot, err)
	}
	for _, bad := range []string{"4/4", "1/1", "0/0", "x", "1-4"} {
		if _, _, err := ParseShard(bad); err == nil {
			t.Fatalf("%s: want error", bad)
		}
	}
}

func TestFilterShard(t *testing.T) {
	type item struct{ label, name string }
	items := []item{
		{"b", "b1"}, {"a", "a1"}, {"c", "c1"},
		{"a", "a2"}, {"b", "b2"}, {"d", "d1"}, {"e", "e1"},
	}
	label := func(it item) string { return it.label }
	names := func(got []item) string {
		var out []string
		for _, it := range got {
			out = append(out, it.label+"/"+it.name)
		}
		return strings.Join(out, ",")
	}
	// Labels sort to a..e; shard 0/2 keeps a, c, e in input order.
	if got := FilterShard(items, label, 0, 2); names(got) != "a/a1,c/c1,a/a2,e/e1" {
		t.Fatalf("0/2: got %s", names(got))
	}
	if got := FilterShard(items, label, 1, 2); names(got) != "b/b1,b/b2,d/d1" {
		t.Fatalf("1/2: got %s", names(got))
	}
	if got := FilterShard(items, label, 0, 1); len(got) != len(items) {
		t.Fatalf("0/1: got %d items, want all %d", len(got), len(items))
	}
	if got := FilterShard(items, label, 0, 0); len(got) != len(items) {
		t.Fatalf("total 0 must not filter, got %d items", len(got))
	}
}

func TestRunStats(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("rusage unsupported on windows")
	}
	stdout, _, stats, err := RunStats([]string{"sh", "-c", "echo hi"}, nil, 30)
	if err != nil {
		t.Fatalf("RunStats: %v", err)
	}
	if string(bytes.TrimSpace(stdout)) != "hi" {
		t.Fatalf("stdout = %q, want hi", stdout)
	}
	if stats.WallS < 0 {
		t.Fatalf("wall = %v, want >= 0", stats.WallS)
	}
	if stats.CpuS < 0 {
		t.Fatalf("cpu = %v, want >= 0", stats.CpuS)
	}
	if stats.MaxRSSKb <= 0 {
		t.Fatalf("maxrss = %d KB, want > 0", stats.MaxRSSKb)
	}
}

func TestRunStatsTimeout(t *testing.T) {
	_, _, stats, err := RunStats([]string{"sh", "-c", "sleep 30"}, nil, 1)
	if _, ok := err.(*TimeoutError); !ok {
		t.Fatalf("err = %v, want TimeoutError", err)
	}
	if stats != (ProcStats{}) {
		t.Fatalf("stats on timeout = %+v, want zero", stats)
	}
}
