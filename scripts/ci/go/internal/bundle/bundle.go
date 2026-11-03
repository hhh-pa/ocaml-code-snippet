// Package bundle builds and decrypts the encrypted test-resource bundle.
//
// It ports tools/build_bundle.sh: tar.xz the recipes superproject's shared
// resources, then OpenPGP-encrypt (AES-256, ASCII-armored) with gpg
// under a fresh or existing passphrase.
package bundle

import (
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// Result reports what was written and, for fresh passphrases, the secret.
type Result struct {
	Out        string
	Bytes      int64
	Fresh      bool
	Passphrase string
}

// Build creates tests/resources.tar.xz.asc from sharedDir.
func Build(root, sharedDir, passFile string) (*Result, error) {
	out := filepath.Join(root, "tests", "resources.tar.xz.asc")
	work, err := os.MkdirTemp("", "snip-bundle-")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(work)

	res := &Result{Out: out}
	actual := passFile
	if actual == "" {
		secret := make([]byte, 48)
		if _, err := rand.Read(secret); err != nil {
			return nil, err
		}
		res.Passphrase = base64.StdEncoding.EncodeToString(secret)
		res.Fresh = true
		actual = filepath.Join(work, "pass.txt")
		if err := os.WriteFile(actual, []byte(res.Passphrase), 0o600); err != nil {
			return nil, err
		}
	} else if _, err := os.Stat(actual); err != nil {
		return nil, fmt.Errorf("passphrase file: %w", err)
	}

	info, err := os.Stat(sharedDir)
	if err != nil || !info.IsDir() {
		return nil, fmt.Errorf("shared dir %q: not a directory", sharedDir)
	}

	ball := filepath.Join(work, "bundle.tar.xz")
	tar := exec.Command("tar", "-cJf", ball,
		"-C", filepath.Dir(sharedDir), filepath.Base(sharedDir))
	if tarOut, err := tar.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("tar: %v: %s", err, tarOut)
	}

	gpg, err := exec.LookPath("gpg")
	if err != nil {
		return nil, fmt.Errorf("gpg not found: install gnupg to build the bundle")
	}

	if err := os.MkdirAll(filepath.Dir(out), 0o755); err != nil {
		return nil, err
	}

	enc := exec.Command(gpg, "--batch", "--yes", "--pinentry-mode", "loopback",
		"--passphrase-file", actual,
		"--symmetric", "--cipher-algo", "AES256", "--armor", "-o", out, ball)
	if combined, err := enc.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("gpg: %v: %s", err, combined)
	}

	st, err := os.Stat(out)
	if err != nil {
		return nil, err
	}
	res.Bytes = st.Size()
	return res, nil
}

// Decrypt unpacks bundlePath into outDir using passphrase.
func Decrypt(bundlePath, passphrase, outDir string) error {
	if passphrase == "" {
		return fmt.Errorf("empty passphrase")
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}

	gpg, err := exec.LookPath("gpg")
	if err != nil {
		return fmt.Errorf("gpg not found: install gnupg to decrypt bundle")
	}

	tar, err := exec.LookPath("tar")
	if err != nil {
		return fmt.Errorf("tar not found")
	}

	work, err := os.MkdirTemp("", "snip-decrypt-")
	if err != nil {
		return err
	}
	defer os.RemoveAll(work)

	tarFile := filepath.Join(work, "bundle.tar.xz")
	dec := exec.Command(gpg, "--batch", "--yes", "--pinentry-mode", "loopback",
		"--passphrase", passphrase,
		"-d", "-o", tarFile, bundlePath)
	if out, err := dec.CombinedOutput(); err != nil {
		return fmt.Errorf("gpg decrypt: %v: %s", err, out)
	}

	extract := exec.Command(tar, "-xJ", "-C", outDir, "--strip-components=1", "-f", tarFile)
	if out, err := extract.CombinedOutput(); err != nil {
		return fmt.Errorf("tar extract: %v: %s", err, out)
	}
	return nil
}

// Report formats the output rotation instructions.
func (r *Result) Report() string {
	out := fmt.Sprintf("wrote %s (%d bytes)\n", r.Out, r.Bytes)
	if r.Fresh {
		out += "FRESH PASSPHRASE (set as RESOURCES_PASSPHRASE, then destroy this copy):\n" +
			r.Passphrase + "\n"
	}
	return out
}
