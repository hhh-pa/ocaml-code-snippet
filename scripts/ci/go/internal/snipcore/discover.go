package snipcore

import (
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// Case is one test_case_N directory with its input/output files.
type Case struct {
	Name   string
	Input  string
	Output string
}

// Found represents one discovered OCaml snippet script and its test cases.
type Found struct {
	Area    string
	Problem string
	Stem    string
	Script  string
	Cases   []Case
	Label   string
	RelStem string
}

// Discover finds all src/**/*.ml scripts and pairs them with test cases in resourcesRoot.
func Discover(root, resourcesRoot string) ([]Found, error) {
	srcDir := filepath.Join(root, "src")
	var scripts []string
	err := filepath.WalkDir(srcDir, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() {
			return nil
		}
		if strings.HasSuffix(entry.Name(), ".ml") {
			scripts = append(scripts, path)
		}
		return nil
	})
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, err
	}
	sort.Strings(scripts)

	var out []Found
	for _, script := range scripts {
		rel, err := filepath.Rel(srcDir, script)
		if err != nil {
			return nil, err
		}
		relStem := strings.TrimSuffix(rel, ".ml")
		// Normalize slashes in relStem for cross-platform consistency
		slashRelStem := filepath.ToSlash(relStem)

		caseDir := filepath.Join(resourcesRoot, filepath.FromSlash(slashRelStem))
		var cases []Case
		entries, err := os.ReadDir(caseDir)
		if err == nil {
			for _, entry := range entries {
				name := entry.Name()
				if !strings.HasPrefix(name, "test_case_") {
					continue
				}
				info, err := entry.Info()
				if err != nil || !info.IsDir() {
					continue
				}
				cases = append(cases, Case{
					Name:   name,
					Input:  filepath.Join(caseDir, name, "input.txt"),
					Output: filepath.Join(caseDir, name, "output.txt"),
				})
			}
			sort.Slice(cases, func(i, j int) bool { return cases[i].Name < cases[j].Name })
		}

		parts := strings.Split(slashRelStem, "/")
		var area, problem, stem string
		if len(parts) >= 2 {
			area = parts[0]
			if len(parts) > 2 {
				problem = strings.Join(parts[1:len(parts)-1], "/")
			} else {
				problem = parts[1]
			}
			stem = parts[len(parts)-1]
		} else {
			area = parts[0]
			problem = parts[0]
			stem = parts[0]
		}

		label := area + "/" + problem
		if problem != stem {
			label = area + "/" + problem + "/" + stem
		}

		out = append(out, Found{
			Area:    area,
			Problem: problem,
			Stem:    stem,
			Script:  script,
			Cases:   cases,
			Label:   label,
			RelStem: slashRelStem,
		})
	}
	return out, nil
}
