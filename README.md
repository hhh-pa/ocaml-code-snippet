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

For automated validation, the shared test cases from the recipes superproject (`resource/shared`) ship as an encrypted bundle, `tests/resources.tar.xz.asc` (tar.xz, AES-256, ASCII-armored). `tests/run_tests.py` runs every `src/**/*.ml` script against the decrypted cases and diff-compares stdout against the expected output (trailing whitespace per line ignored).

### CI/CD Pipeline

`.github/workflows/test.yml` validates the snippets on push to `master`, pull requests, and manual dispatch. It decrypts the bundle with the `RESOURCES_PASSPHRASE` repository secret and runs:

```sh
python3 tests/run_tests.py --resources tests/resources --jobs 4 --timeout 300
```

To test locally, decrypt the bundle into `tests/resources/` (passphrase holders only) and run the same command:

```sh
mkdir -p tests/resources
gpg --batch --yes --pinentry-mode loopback --passphrase "$RESOURCES_PASSPHRASE" \
  -d tests/resources.tar.xz.asc | tar -xJ -C tests/resources --strip-components=1
python3 tests/run_tests.py --resources tests/resources --jobs 4 --timeout 300
```

Rebuild the bundle after resource changes with:

```sh
tools/build_bundle.sh <resource/shared dir> [passphrase-file]
```

See `tools/build_bundle.sh` for rotation instructions when using a fresh passphrase.

## Performance Tracking (Callgrind)

`tools/measure.py` measures load-independent algorithmic complexity using Valgrind Callgrind:

- `instructions` (`Ir` — simulated instruction reads) — completely load-invariant and deterministic across runs and runners.
- `cpu_time` (user + sys CPU time)
- `wall_time`

```sh
python3 tools/measure.py --resources tests/resources --repeat 1 \
  --output perf.json --bmf-output bencher.json
python3 tools/measure.py --self-test
```

CI runs correctness across sharded runners in the `test-shard` jobs, aggregated by `test`. Then the `perf-callgrind` job measures instruction counts under Valgrind Callgrind across 4 shards, reporting to Bencher under testbed `ubuntu-latest-ocaml` and measure `instructions` (gated by `BENCHER_API_KEY`). A fan-in `report` job merges all shard results, verifies equality against the plain BMF union (`jq -s 'add'`), and publishes a combined Markdown summary table to the GitHub Actions job summary.

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
