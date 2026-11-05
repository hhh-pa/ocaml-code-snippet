# ocaml-code-snippet

## Introduction

This repository is a curated collection of commonly used algorithms, aimed at reinforcing a software engineer's general algorithmic knowledge. Each snippet is implemented in a minimal and accessible manner, focusing on clarity of logic and optimal time/space complexity rather than production-readiness.

This repository is the OCaml port of the collection (see [cpp-code-snippets](https://github.com/m-hu/cpp-code-snippets), [clojure-code-snippet](https://github.com/hhh-pa/clojure-code-snippet), and [python-code-snippet](https://github.com/hhh-pa/python-code-snippet)). Solutions mirror the other snippet repos: one standalone script per problem under `src/<area>/<problem>.ml` (or `src/<area>/<problem>/<stem>.ml`), compiled via `ocamlopt -O2` or executed directly via `ocaml <script>` (stdin in, stdout out).

### Scope

The repository intentionally excludes production-level concerns such as:

- Memory management
- Error handling
- Vulnerability mitigation

These aspects, while important in real-world applications, are intentionally left out to keep the focus on core algorithmic logic. The assumption is that the intended viewers are primarily interested in the conceptual content itself, rather than scrutinizing the code for potential attack surfaces or production-level robustness.

### Privacy & Exposure

Some code examples have been moved to a **private repository** as a precautionary measure to limit exposure and reduce potential risk, especially when the repository is accessed in contexts where the intent of the viewer is uncertain.

### Test Cases

To respect the rights of original content creators, **test cases are not included** in this repository in plain form. Instead, they can be found on the websites where the referenced problems originate.

For automated validation, the shared test cases from the recipes superproject (`resource/shared`) ship as an encrypted bundle, `tests/resources.tar.xz.asc` (tar.xz, AES-256, ASCII-armored). All testing and measuring tools are written in Go under `scripts/ci/go` (compiled into a single cached `snip` binary). `snip test` runs every `src/**/*.ml` script against the decrypted cases and diff-compares stdout against the expected output (trailing whitespace per line ignored).

### CI/CD Pipeline

`.github/workflows/test.yml` validates the snippets on push to `master`, pull requests, and manual dispatch. It decrypts the bundle with the `RESOURCES_PASSPHRASE` organization secret (shared with the clojure/python snippet repos) and runs:

```sh
snip test --resources tests/resources --jobs 4 --timeout 300
```

To test locally, decrypt the bundle into `tests/resources/` (passphrase holders only) and run the same command:

```sh
mkdir -p tests/resources
gpg --batch --yes --pinentry-mode loopback --passphrase "$RESOURCES_PASSPHRASE" \
  -d tests/resources.tar.xz.asc | tar -xJ -C tests/resources --strip-components=1
snip test --resources tests/resources --jobs 4 --timeout 300
```

Rebuild the bundle after resource changes with:

```sh
snip bundle <resource/shared dir> [passphrase-file]
```

See `snip bundle --help` for rotation instructions when using a fresh passphrase.

## Performance Tracking (Pin)

`snip measure --pin` measures load-independent algorithmic complexity using Intel Pin 4.4 with a vendored inscount tool (`scripts/ci/instrumentation/`). Each case is measured once under Pin, and that single instrumented run yields two signals:

- `pin_instructions` (executed application instructions) — completely load-invariant and deterministic across runs and runners sharing one Pin version.
- `allocated_bytes` (total allocation volume in bytes, from `Gc.allocated_bytes`) — the benchmark binary reports it on stderr at exit via a linked reporter module (`scripts/ci/measure/snip_gc_report.ml`); the harness parses it from the same Pin run that yields the instruction count.

`--repeat 0` selects this pin-only mode: no timed phase runs, correctness is still gated on the Pin run's stdout, and reports carry `Runs: 0` with no time summaries (the BMF and summary table omit `cpu_time` rather than emitting zeros). Peak RSS is deliberately not reported from instrumented runs: under Pin the resident set is dominated by Pin's own footprint, not the snippet.

```sh
scripts/ci/setup-instrumentation.sh  # one-time Pin 4.4 kit + tool provisioning
snip measure --resources tests/resources --repeat 0 \
  --pin --pin-bin "$INSTRUMENTATION_BIN" --pin-tool "$INSTRUMENTATION_TOOL" \
  --output perf.json --bmf-output bencher.json
snip measure --self-test
```

CI runs correctness across sharded runners in the `test-shard` jobs, aggregated by `test`. Then the `perf-instrumentation` job provisions Pin 4.4 (kit + built tool cached across runs) and measures instruction counts plus allocated bytes across 4 shards (`--repeat 0`), reporting to Bencher under testbed `ubuntu-latest-ocaml` and measure `pin_instructions` (gated by `BENCHER_API_KEY`). A fan-in `report-instrumentation` job merges all shard results, verifies equality against the plain BMF union (`jq -s 'add'`), and publishes a combined Markdown summary table to the GitHub Actions job summary.

Note: the old Callgrind `instructions` (`Ir`) measure ends with the Pin switch — Pin counts and Callgrind Ir are different signals on different scales, so Bencher history restarts under `pin_instructions` by design. The Callgrind code path stays in the harness for local runs with valgrind installed.

## Categories (393 snippets)

- **combinatorics** (18 snippets)
- **compiler** (19 snippets)
- **computational_geometry** (24 snippets)
- **data_structure** (40 snippets)
- **distributed** (27 snippets)
- **dp** (35 snippets)
- **finance** (23 snippets)
- **graph** (39 snippets)
- **math** (36 snippets)
- **optics** (6 snippets)
- **search** (43 snippets)
- **sort** (28 snippets)
- **string_related** (35 snippets)
- **systems** (20 snippets)
