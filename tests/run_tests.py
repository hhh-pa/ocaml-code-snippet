#!/usr/bin/env python3
"""Self-contained correctness runner for the OCaml snippet repo.

For every solution script under src/<area>/<problem>/<file>.ml this runs
either the compiled native binary (via ocamlopt) or `ocaml <file>.ml` once
per shared test case and diff-compares stdout against the expected output
(trailing whitespace per line ignored, same as the recipes superproject harness).

Resources live OUTSIDE this repo (they belong to the recipes superproject).
Layout expected under --resources DIR:

    DIR/<area>/<problem>/<file-stem>/test_case_N/input.txt
    DIR/<area>/<problem>/<file-stem>/test_case_N/output.txt

Usage:
    python3 tests/run_tests.py --resources tests/resources \
        [--jobs N] [--timeout 120] [--only substring] [--shard I/N] [--no-compile]

Shard I/N runs only the deterministic slice of the sorted task list
(task index mod N == I-1), so CI can split the suite across jobs.

Exit status is non-zero when any case fails or a case is missing resources.
"""

import argparse
import glob
import os
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def normalize(content):
    return b"\n".join(line.strip() for line in content.splitlines())


def discover(resources_root):
    """Yield (area, problem, stem, script_path, [(case_name, in, out), ...])."""
    for script in sorted(glob.glob(os.path.join(ROOT, "src", "**", "*.ml"), recursive=True)):
        rel = os.path.relpath(script, os.path.join(ROOT, "src"))
        rel_stem = os.path.splitext(rel)[0]
        case_dir = os.path.join(resources_root, rel_stem)
        cases = []
        for case in sorted(glob.glob(os.path.join(case_dir, "test_case_*"))):
            if os.path.isdir(case):
                cases.append(
                    (
                        os.path.basename(case),
                        os.path.join(case, "input.txt"),
                        os.path.join(case, "output.txt"),
                    )
                )
        parts = rel_stem.split(os.sep)
        if len(parts) >= 2:
            area = parts[0]
            problem = "/".join(parts[1:-1]) if len(parts) > 2 else parts[1]
            stem = parts[-1]
        else:
            area, problem, stem = parts[0], parts[0], parts[0]
        yield area, problem, stem, script, cases


def compile_snippet(script_path, bin_path, obj_dir):
    """Compile an OCaml script to a native binary using ocamlopt."""
    os.makedirs(os.path.dirname(bin_path), exist_ok=True)
    os.makedirs(obj_dir, exist_ok=True)
    cmd = ["ocamlopt", "-O2", "-o", os.path.abspath(bin_path), os.path.abspath(script_path)]
    proc = subprocess.run(cmd, cwd=obj_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if proc.returncode != 0:
        err = proc.stderr.decode(errors="ignore").strip()
        return f"compile error (exit {proc.returncode}): {err}"
    return None


def run_case(cmd, case_name, input_path, output_path, timeout):
    with open(input_path, "rb") as fd:
        try:
            proc = subprocess.run(
                cmd,
                stdin=fd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=timeout,
            )
        except subprocess.TimeoutExpired:
            return f"TIMEOUT (>{timeout}s)"
    if proc.returncode != 0:
        tail = proc.stderr.decode(errors="ignore").strip().splitlines()
        return "exit {}: {}".format(proc.returncode, tail[-1] if tail else "no stderr")
    with open(output_path, "rb") as fd:
        expected = normalize(fd.read())
    if normalize(proc.stdout) != expected:
        got = normalize(proc.stdout).decode(errors="ignore")
        want = expected.decode(errors="ignore")
        return "output mismatch: expected {!r} got {!r}".format(want[:200], got[:200])
    return None


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--resources", default=os.path.join(ROOT, "tests", "resources"))
    parser.add_argument("--jobs", type=int, default=4)
    parser.add_argument("--timeout", type=int, default=120)
    parser.add_argument("--only", default="", help="substring filter on area/problem")
    parser.add_argument("--shard", default="", help="shard slice I/N (1-based I)")
    parser.add_argument("--no-compile", action="store_true", help="run with ocaml interpreter instead of ocamlopt")
    args = parser.parse_args(argv)

    shard_index, shard_total = 0, 1
    if args.shard:
        try:
            picked, total = args.shard.split("/")
            shard_index, shard_total = int(picked) - 1, int(total)
        except ValueError:
            raise SystemExit("bad --shard {!r}: want I/N".format(args.shard))
        if not 0 <= shard_index < shard_total:
            raise SystemExit("bad --shard {!r}: want 1 <= I <= N".format(args.shard))

    has_ocamlopt = (shutil.which("ocamlopt") is not None) and not args.no_compile
    has_ocaml = shutil.which("ocaml") is not None

    if not has_ocamlopt and not has_ocaml:
        print("warning: neither ocamlopt nor ocaml found in PATH", file=sys.stderr)

    tasks = []
    missing = []
    script_map = {}  # script -> list of tasks

    for area, problem, stem, script, cases in discover(args.resources):
        label = f"{area}/{problem}/{stem}" if problem != stem else f"{area}/{problem}"
        if args.only and args.only not in label and args.only not in script:
            continue
        if not cases:
            missing.append(label)
            continue
        for case_name, inp, outp in cases:
            tasks.append((label, case_name, script, inp, outp))

    tasks = sorted(tasks)
    if shard_total > 1:
        tasks = [task for index, task in enumerate(tasks) if index % shard_total == shard_index]
    total = len(tasks)
    needed_scripts = sorted({t[2] for t in tasks})
    print("ocaml tests: {} cases{} ({} problems, {} snippets)".format(
        total,
        " [shard {}/{}]".format(shard_index + 1, shard_total) if shard_total > 1 else "",
        len({label for label, _, _, _, _ in tasks}),
        len(needed_scripts),
    ), flush=True)

    # Compile needed scripts if ocamlopt is available
    binaries = {}
    compile_failures = {}
    if has_ocamlopt:
        print(f"Compiling {len(needed_scripts)} snippets with ocamlopt -O2...", flush=True)
        build_base = os.path.join(ROOT, "_build")
        def _compile(script):
            rel = os.path.relpath(script, os.path.join(ROOT, "src"))
            rel_stem = os.path.splitext(rel)[0]
            bin_path = os.path.join(build_base, "bin", rel_stem)
            obj_dir = os.path.join(build_base, "obj", rel_stem)
            err = compile_snippet(script, bin_path, obj_dir)
            return script, bin_path, err

        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            futures = [pool.submit(_compile, s) for s in needed_scripts]
            for fut in as_completed(futures):
                script, bin_path, err = fut.result()
                if err:
                    compile_failures[script] = err
                else:
                    binaries[script] = bin_path

    failures = []
    passed = 0
    done = 0

    run_tasks = []
    for label, case_name, script, inp, outp in tasks:
        if script in compile_failures:
            failures.append("{}/{}: {}".format(label, case_name, compile_failures[script]))
            print("FAIL {}/{}: {}".format(label, case_name, compile_failures[script]), flush=True)
            done += 1
            continue
        if has_ocamlopt and script in binaries:
            cmd = [binaries[script]]
        else:
            cmd = ["ocaml", script]
        run_tasks.append((label, case_name, cmd, inp, outp))

    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = {
            pool.submit(run_case, cmd, case, inp, outp, args.timeout): (label, case)
            for label, case, cmd, inp, outp in run_tasks
        }
        for future in as_completed(futures):
            label, case = futures[future]
            error = future.result()
            done += 1
            if error is None:
                passed += 1
                print("PASS {}/{} [{}/{}]".format(label, case, done, total), flush=True)
            else:
                failures.append("{}/{}: {}".format(label, case, error))
                print("FAIL {}/{} [{}/{}]: {}".format(label, case, done, total, error), flush=True)

    for label in missing:
        failures.append("{}: no resources found".format(label))
    for failure in sorted(failures):
        print("FAIL", failure)
    print(
        "ocaml tests: {} passed, {} failed, {} missing-resource problems".format(
            passed, len(failures) - len(missing), len(missing)
        )
    )
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
