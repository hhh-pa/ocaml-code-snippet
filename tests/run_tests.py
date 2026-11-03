#!/usr/bin/env python3
"""Self-contained runner and compiler for the OCaml snippet repo.

Provides separate compilation and test execution phases with granular
per-snippet compilation timing, compiler warning tracking, per-test
execution timing, and comprehensive summaries.

Resources live OUTSIDE this repo (they belong to the recipes superproject).
Layout expected under --resources DIR:

    DIR/<area>/<problem>/<file-stem>/test_case_N/input.txt
    DIR/<area>/<problem>/<file-stem>/test_case_N/output.txt

Usage:
    python3 tests/run_tests.py --resources tests/resources \
        [--jobs N] [--timeout 120] [--only substring] [--shard I/N] \
        [--compile-only] [--test-only] [--no-compile]

Options:
    --compile-only  Only compile needed snippets; report compile times & warnings
    --test-only     Only execute tests using previously compiled binaries
    --no-compile    Run using the ocaml interpreter instead of native ocamlopt
    --shard I/N     Run deterministic slice (1 <= I <= N)
"""

import argparse
import glob
import os
import shutil
import subprocess
import sys
import time
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
    """Compile an OCaml script to a native binary using ocamlopt.

    Returns (elapsed_seconds, warnings_list, error_or_none).
    """
    os.makedirs(os.path.dirname(bin_path), exist_ok=True)
    os.makedirs(obj_dir, exist_ok=True)
    cmd = ["ocamlopt", "-O2", "-o", os.path.abspath(bin_path), os.path.abspath(script_path)]

    t0 = time.perf_counter()
    try:
        proc = subprocess.run(cmd, cwd=obj_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    except FileNotFoundError:
        return 0.0, [], f"compiler not found: {cmd[0]}"
    elapsed = time.perf_counter() - t0

    stderr_text = proc.stderr.decode(errors="ignore").strip()
    warnings = []
    if "Warning" in stderr_text or "warning" in stderr_text:
        warnings = [line for line in stderr_text.splitlines() if "Warning" in line or "warning" in line]

    if proc.returncode != 0:
        return elapsed, warnings, f"compile error (exit {proc.returncode}):\n{stderr_text}"
    return elapsed, warnings, None


def run_case(cmd, case_name, input_path, output_path, timeout):
    """Run a single test case. Returns (elapsed_seconds, error_or_none)."""
    with open(input_path, "rb") as fd:
        t0 = time.perf_counter()
        try:
            proc = subprocess.run(
                cmd,
                stdin=fd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=timeout,
            )
        except subprocess.TimeoutExpired:
            elapsed = time.perf_counter() - t0
            return elapsed, f"TIMEOUT (>{timeout}s)"
        except FileNotFoundError:
            elapsed = time.perf_counter() - t0
            return elapsed, f"executable not found: {cmd[0]}"
    elapsed = time.perf_counter() - t0

    if proc.returncode != 0:
        tail = proc.stderr.decode(errors="ignore").strip().splitlines()
        return elapsed, "exit {}: {}".format(proc.returncode, tail[-1] if tail else "no stderr")
    with open(output_path, "rb") as fd:
        expected = normalize(fd.read())
    if normalize(proc.stdout) != expected:
        got = normalize(proc.stdout).decode(errors="ignore")
        want = expected.decode(errors="ignore")
        return elapsed, "output mismatch: expected {!r} got {!r}".format(want[:200], got[:200])
    return elapsed, None


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--resources", default=os.path.join(ROOT, "tests", "resources"))
    parser.add_argument("--jobs", type=int, default=4)
    parser.add_argument("--timeout", type=int, default=120)
    parser.add_argument("--only", default="", help="substring filter on area/problem")
    parser.add_argument("--shard", default="", help="shard slice I/N (1-based I)")
    parser.add_argument("--no-compile", action="store_true", help="run with ocaml interpreter instead of ocamlopt")
    parser.add_argument("--compile-only", action="store_true", help="only compile snippets, report compile timing")
    parser.add_argument("--test-only", action="store_true", help="only run test cases with existing binaries")
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
    snippet_labels = {}  # script -> label

    for area, problem, stem, script, cases in discover(args.resources):
        label = f"{area}/{problem}/{stem}" if problem != stem else f"{area}/{problem}"
        if args.only and args.only not in label and args.only not in script:
            continue
        snippet_labels[script] = label
        if not cases:
            missing.append(label)
            continue
        for case_name, inp, outp in cases:
            tasks.append((label, case_name, script, inp, outp))

    tasks = sorted(tasks)
    if shard_total > 1:
        tasks = [task for index, task in enumerate(tasks) if index % shard_total == shard_index]
    total_cases = len(tasks)
    needed_scripts = sorted({t[2] for t in tasks})
    shard_str = " [shard {}/{}]".format(shard_index + 1, shard_total) if shard_total > 1 else ""

    print(f"OCaml suite{shard_str}: {total_cases} test cases across {len(needed_scripts)} snippets", flush=True)

    build_base = os.path.join(ROOT, "_build")
    binaries = {}
    compile_failures = {}
    compile_timings = {}
    compile_warnings = {}

    # Determine binary path for each script
    for s in needed_scripts:
        rel = os.path.relpath(s, os.path.join(ROOT, "src"))
        rel_stem = os.path.splitext(rel)[0]
        bin_path = os.path.join(build_base, "bin", rel_stem)
        binaries[s] = bin_path

    # =========================================================================
    # PHASE 1: COMPILATION
    # =========================================================================
    if not args.test_only:
        if has_ocamlopt:
            print(f"\n=== Compilation Phase (ocamlopt -O2, jobs={args.jobs}) ===", flush=True)
            t_comp_start = time.perf_counter()

            def _compile_task(script):
                rel = os.path.relpath(script, os.path.join(ROOT, "src"))
                rel_stem = os.path.splitext(rel)[0]
                bin_path = binaries[script]
                obj_dir = os.path.join(build_base, "obj", rel_stem)
                elapsed, warns, err = compile_snippet(script, bin_path, obj_dir)
                return script, elapsed, warns, err

            done_comp = 0
            with ThreadPoolExecutor(max_workers=args.jobs) as pool:
                futures = [pool.submit(_compile_task, s) for s in needed_scripts]
                for fut in as_completed(futures):
                    script, elapsed, warns, err = fut.result()
                    done_comp += 1
                    label = snippet_labels.get(script, script)
                    compile_timings[script] = elapsed
                    if warns:
                        compile_warnings[script] = warns

                    if err:
                        compile_failures[script] = err
                        print(f"FAIL COMPILE {label} [{elapsed:.3f}s] [{done_comp}/{len(needed_scripts)}]:\n{err}", flush=True)
                    else:
                        warn_tag = f" ({len(warns)} warnings)" if warns else ""
                        print(f"COMPILED     {label} [{elapsed:.3f}s]{warn_tag} [{done_comp}/{len(needed_scripts)}]", flush=True)
                        if warns:
                            for w in warns:
                                print(f"  warning: {w}", flush=True)

            t_comp_wall = time.perf_counter() - t_comp_start
            sum_comp_time = sum(compile_timings.values())
            avg_comp = sum_comp_time / len(needed_scripts) if needed_scripts else 0.0

            print("\n=== Compilation Summary ===")
            print(f"Total snippets : {len(needed_scripts)}")
            print(f"Compiled       : {len(needed_scripts) - len(compile_failures)} in {t_comp_wall:.2f}s wall time (sum {sum_comp_time:.2f}s, avg {avg_comp:.3f}s/snippet)")
            print(f"Compile errors : {len(compile_failures)}")
            print(f"Snippets w/warn: {len(compile_warnings)}")
            print("===========================\n", flush=True)

            if compile_failures and args.compile_only:
                return 1
        elif args.compile_only:
            print("warning: ocamlopt not found; skipping native compilation", file=sys.stderr)
            return 0

    if args.compile_only:
        return 1 if compile_failures else 0

    # =========================================================================
    # PHASE 2: TEST EXECUTION
    # =========================================================================
    print(f"\n=== Test Execution Phase (jobs={args.jobs}, timeout={args.timeout}s) ===", flush=True)
    t_test_start = time.perf_counter()

    failures = []
    passed = 0
    done = 0
    test_timings = []

    run_tasks = []
    for label, case_name, script, inp, outp in tasks:
        if script in compile_failures:
            failures.append("{}/{}: {}".format(label, case_name, compile_failures[script]))
            print("FAIL {}/{}: {}".format(label, case_name, compile_failures[script]), flush=True)
            done += 1
            continue
        if has_ocamlopt and os.path.isfile(binaries[script]):
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
            elapsed, error = future.result()
            done += 1
            test_timings.append(elapsed)
            if error is None:
                passed += 1
                print("PASS {}/{} in {:.3f}s [{}/{}]".format(label, case, elapsed, done, total_cases), flush=True)
            else:
                failures.append("{}/{}: {}".format(label, case, error))
                print("FAIL {}/{} in {:.3f}s [{}/{}]: {}".format(label, case, elapsed, done, total_cases, error), flush=True)

    t_test_wall = time.perf_counter() - t_test_start
    sum_test_time = sum(test_timings)
    avg_test = sum_test_time / len(test_timings) if test_timings else 0.0

    for label in missing:
        failures.append("{}: no resources found".format(label))
    for failure in sorted(failures):
        print("FAIL", failure)

    print("\n=== Test Execution Summary ===")
    print(f"Total test cases : {total_cases}")
    print(f"Passed           : {passed} in {t_test_wall:.2f}s wall time (sum {sum_test_time:.2f}s, avg {avg_test:.4f}s/test)")
    print(f"Failed           : {len(failures) - len(missing)}")
    print(f"Missing resources: {len(missing)}")
    if not args.test_only and has_ocamlopt:
        print(f"Total pipeline   : {t_comp_wall + t_test_wall:.2f}s (compilation: {t_comp_wall:.2f}s, tests: {t_test_wall:.2f}s)")
    print("==============================\n", flush=True)

    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
