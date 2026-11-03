package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"

	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/bundle"
	"github.com/hhh-pa/ocaml-code-snippet/scripts/ci/go/internal/snipcore"
)

const bundleUsage = `Build (or rebuild) the encrypted test-resource bundle from the recipes
superproject, and print the rotation instructions.

Usage: snip bundle <resource/shared dir> [passphrase-file]

With no passphrase file, a fresh 64-char passphrase is generated and
printed ONCE (save it, then set it as the RESOURCES_PASSPHRASE secret).
With a passphrase file, the bundle is re-encrypted under the existing
secret value.

Output: tests/resources.tar.xz.asc (tar.xz, AES-256, ASCII-armored).
`

func runBundle(argv []string) int {
	if len(argv) == 1 && (argv[0] == "-h" || argv[0] == "--help") {
		fmt.Print(bundleUsage)
		return 0
	}
	positionals, err := parseFlags("bundle", argv, flagSpec{})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(bundleUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}
	if len(positionals) < 1 || len(positionals) > 2 {
		fmt.Fprintln(os.Stderr, "snip bundle: usage: snip bundle <resource/shared dir> [passphrase-file]")
		return 2
	}
	shared := positionals[0]
	passFile := ""
	if len(positionals) == 2 {
		passFile = positionals[1]
	}
	res, err := bundle.Build(snipcore.RepoRoot(), shared, passFile)
	if err != nil {
		fmt.Fprintln(os.Stderr, "snip bundle:", err)
		return 1
	}
	fmt.Print(res.Report())
	return 0
}

const decryptUsage = `Decrypt and extract tests/resources.tar.xz.asc into tests/resources.

Usage: snip decrypt [--bundle PATH] [--output DIR] [--passphrase-env VAR]

By default reads passphrase from $RESOURCES_PASSPHRASE and extracts to tests/resources.
`

func runDecrypt(argv []string) int {
	root := snipcore.RepoRoot()
	bundlePath := filepath.Join(root, "tests", "resources.tar.xz.asc")
	outputDir := filepath.Join(root, "tests", "resources")
	passEnv := "RESOURCES_PASSPHRASE"

	_, err := parseFlags("decrypt", argv, flagSpec{
		Strs: map[string]*string{
			"bundle":         &bundlePath,
			"output":         &outputDir,
			"passphrase-env": &passEnv,
		},
	})
	if err != nil {
		if errors.Is(err, errHelp) {
			fmt.Print(decryptUsage)
			return 0
		}
		fmt.Fprintln(os.Stderr, err)
		return 2
	}

	passphrase := os.Getenv(passEnv)
	if passphrase == "" {
		fmt.Fprintf(os.Stderr, "snip decrypt: %s environment variable is not configured\n", passEnv)
		return 1
	}

	if err := bundle.Decrypt(bundlePath, passphrase, outputDir); err != nil {
		fmt.Fprintf(os.Stderr, "snip decrypt: %v\n", err)
		return 1
	}
	fmt.Printf("Extracted %s to %s\n", bundlePath, outputDir)
	return 0
}
