package snipcore

import (
	"bytes"
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

func TestParseTestShard(t *testing.T) {
	idx, tot, err := ParseTestShard("1/4")
	if err != nil || idx != 0 || tot != 4 {
		t.Fatalf("1/4: got (%d, %d, %v)", idx, tot, err)
	}
	idx, tot, err = ParseTestShard("4/4")
	if err != nil || idx != 3 || tot != 4 {
		t.Fatalf("4/4: got (%d, %d, %v)", idx, tot, err)
	}
	if _, _, err := ParseTestShard("0/4"); err == nil {
		t.Fatalf("0/4: want error")
	}
	if _, _, err := ParseTestShard("5/4"); err == nil {
		t.Fatalf("5/4: want error")
	}
}

func TestParseMeasureShard(t *testing.T) {
	idx, tot, err := ParseMeasureShard("0/4")
	if err != nil || idx != 0 || tot != 4 {
		t.Fatalf("0/4: got (%d, %d, %v)", idx, tot, err)
	}
	idx, tot, err = ParseMeasureShard("3/4")
	if err != nil || idx != 3 || tot != 4 {
		t.Fatalf("3/4: got (%d, %d, %v)", idx, tot, err)
	}
	if _, _, err := ParseMeasureShard("4/4"); err == nil {
		t.Fatalf("4/4: want error")
	}
}
