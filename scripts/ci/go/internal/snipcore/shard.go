package snipcore

import (
	"errors"
	"sort"
	"strconv"
	"strings"
)

// ParseShard parses the 0-based K/N --shard flag shared by snip test
// and snip measure, returning the index and total.
func ParseShard(text string) (index, total int, err error) {
	picked, total, ok := splitShard(text)
	if !ok {
		return 0, 0, errors.New("bad --shard '" + text + "': want K/N")
	}
	if total < 1 || picked < 0 || picked >= total {
		return 0, 0, errors.New("bad --shard '" + text + "': need 0 <= K < N")
	}
	return picked, total, nil
}

// FilterShard keeps the items whose label falls in shard index/total:
// labels are sorted and dealt round-robin across shards, so a label's
// items always stay together (cohesive sharding) and every subcommand
// splits the suite identically. Item order is preserved. Callers
// parse the K/N flag with ParseShard first.
func FilterShard[T any](items []T, label func(T) string, index, total int) []T {
	if total < 1 {
		return items
	}
	seen := map[string]bool{}
	var groupNames []string
	for _, it := range items {
		if l := label(it); !seen[l] {
			seen[l] = true
			groupNames = append(groupNames, l)
		}
	}
	sort.Strings(groupNames)

	keep := map[string]bool{}
	for i, name := range groupNames {
		if i%total == index {
			keep[name] = true
		}
	}
	var out []T
	for _, it := range items {
		if keep[label(it)] {
			out = append(out, it)
		}
	}
	return out
}

func splitShard(text string) (picked, total int, ok bool) {
	parts := strings.Split(text, "/")
	if len(parts) != 2 {
		return 0, 0, false
	}
	picked, err1 := strconv.Atoi(strings.TrimSpace(parts[0]))
	total, err2 := strconv.Atoi(strings.TrimSpace(parts[1]))
	if err1 != nil || err2 != nil {
		return 0, 0, false
	}
	return picked, total, true
}
