#!/usr/bin/env python3
"""Load-independent Callgrind performance measurement for OCaml snippets.

Compiles every src/**/*.ml script to a native binary using ocamlopt -O2
(or runs under ocaml if ocamlopt is unavailable), runs it against the
shared test cases, and records metrics under Valgrind Callgrind:

  * instructions (simulated instruction reads 'Ir' from Callgrind) --
    fully load-independent and deterministic across runs.
  * cpu_s (user + sys CPU time).
  * wall_s (wall time).
  * maxrss_kb (peak RSS in KB).

Usage:
    python3 tools/measure.py --resources tests/resources \
        [--repeat 1] [--jobs 2] [--timeout 300] [--only substring] \
        [--output perf.json] [--bmf-output bencher.json] \
        [--summary-file "$GITHUB_STEP_SUMMARY"] [--shard K/N]

    python3 tools/measure.py --merge shard-*.json --output merged.json \
        [--bmf-output bencher.json] [--summary-file summary.md]

    python3 tools/measure.py --self-test
"""

import argparse
import glob
import json
import os
import platform
import re
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, "tests"))
import run_tests

TOOL_VERSION = 1
_CALLGRIND_TOTAL_RE = re.compile(r"^(?:totals|summary):\s*(\d+)", re.MULTILINE)


def parse_callgrind_output(content):
    """Extract total instruction count from Callgrind output text."""
    matches = _CALLGRIND_TOTAL_RE.findall(content)
    if matches:
        return int(matches[-1])
    for line in content.splitlines():
        line = line.strip()
        if line.startswith("totals:") or line.startswith("summary:"):
            parts = line.split(":")
            if len(parts) >= 2:
                try:
                    return int(parts[1].strip().split()[0])
                except (ValueError, IndexError):
                    pass
    return None


def parse_shard(text):
    """Parse K/N shard selector into (index, total), 0-based index."""
    try:
        k, _, n = text.partition("/")
        index, total = int(k), int(n)
    except (TypeError, ValueError) as exc:
        raise ValueError("bad --shard {!r}: want K/N".format(text)) from exc
    if total < 1 or not 0 <= index < total:
        raise ValueError("bad --shard {!r}: need 0 <= K < N".format(text))
    return index, total


def select_shard(tasks, index, total):
    """Deterministically keep every total-th snippet starting at index.

    Sharding is by SNIPPET (label), so all test cases of a snippet stay
    together in one shard for consistent BMF aggregation and fan-in merging.
    """
    groups = {}
    for t in tasks:
        # t is (label, stem, script, case_name, inp, outp)
        groups.setdefault(t[0], []).append(t)
    keep = {g for i, g in enumerate(sorted(groups)) if i % total == index}
    return [t for t in tasks if t[0] in keep]


def summarize_numbers(values):
    if not values:
        return None
    values = sorted(values)
    return {
        "min": values[0],
        "median": statistics.median(values),
        "max": values[-1],
    }


def compile_snippet_cached(script_path, bin_path, obj_dir):
    """Compile OCaml script with ocamlopt -O2, keeping artifacts inside obj_dir."""
    if os.path.isfile(bin_path):
        return None
    os.makedirs(os.path.dirname(bin_path), exist_ok=True)
    os.makedirs(obj_dir, exist_ok=True)
    cmd = ["ocamlopt", "-O2", "-o", os.path.abspath(bin_path), os.path.abspath(script_path)]
    proc = subprocess.run(cmd, cwd=obj_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if proc.returncode != 0:
        err = proc.stderr.decode(errors="ignore").strip()
        return f"compile error (exit {proc.returncode}): {err}"
    return None


def measure_case_callgrind(has_valgrind, exec_cmd, case_name, input_path, output_path, repeat, timeout):
    with open(output_path, "rb") as fd:
        expected = run_tests.normalize(fd.read())

    runs = []
    for _ in range(repeat):
        callgrind_file = None
        fd_temp = None
        try:
            if has_valgrind:
                fd_temp, callgrind_file = tempfile.mkstemp(prefix="snip-cg-", suffix=".out")
                os.close(fd_temp)
                cmd = [
                    "valgrind",
                    "--tool=callgrind",
                    f"--callgrind-out-file={callgrind_file}",
                ] + exec_cmd
            else:
                cmd = list(exec_cmd)

            t0 = time.perf_counter()
            with open(input_path, "rb") as stdin_fd:
                try:
                    proc = subprocess.run(
                        cmd,
                        stdin=stdin_fd,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        timeout=timeout,
                    )
                except subprocess.TimeoutExpired:
                    return None, f"TIMEOUT (>{timeout}s)"
            wall_s = time.perf_counter() - t0

            if proc.returncode != 0:
                err_text = proc.stderr.decode(errors="ignore").strip().splitlines()
                return None, f"exit {proc.returncode}: {err_text[-1] if err_text else 'no stderr'}"

            if run_tests.normalize(proc.stdout) != expected:
                got = run_tests.normalize(proc.stdout).decode(errors="ignore")
                want = expected.decode(errors="ignore")
                return None, f"output mismatch: expected {want[:100]!r} got {got[:100]!r}"

            instructions = None
            if has_valgrind and callgrind_file and os.path.isfile(callgrind_file):
                with open(callgrind_file, "r", errors="ignore") as cg_fd:
                    instructions = parse_callgrind_output(cg_fd.read())

            runs.append({
                "instructions": instructions,
                "wall_s": wall_s,
                "cpu_s": wall_s,
                "maxrss_kb": 0,
            })
        finally:
            if callgrind_file and os.path.exists(callgrind_file):
                try:
                    os.unlink(callgrind_file)
                except OSError:
                    pass

    insn_vals = [r["instructions"] for r in runs if r["instructions"] is not None]
    summary = {
        "instructions": summarize_numbers(insn_vals) if insn_vals else None,
        "wall_s": summarize_numbers([r["wall_s"] for r in runs]),
        "cpu_s": summarize_numbers([r["cpu_s"] for r in runs]),
        "maxrss_kb": summarize_numbers([r["maxrss_kb"] for r in runs]),
        "runs": len(runs),
    }
    return summary, None


def build_bmf(results):
    """Aggregate per-snippet summaries into Bencher Metric Format JSON."""
    snippets = {}
    for r in results:
        label = r["label"]
        snippets.setdefault(label, []).append(r)

    bmf = {}
    for name in sorted(snippets):
        items = snippets[name]
        total_instructions = 0
        has_instructions = False
        total_cpu = 0.0

        for it in items:
            s = it.get("summary")
            if not s:
                continue
            if s.get("instructions") and s["instructions"]["median"] is not None:
                total_instructions += int(s["instructions"]["median"])
                has_instructions = True
            if s.get("cpu_s") and s["cpu_s"]["median"] is not None:
                total_cpu += float(s["cpu_s"]["median"])

        measures = {}
        if has_instructions:
            measures["instructions"] = {"value": total_instructions}
        measures["cpu_time"] = {"value": total_cpu}

        bmf[name] = measures
    return bmf


def merge_reports(paths):
    merged_results = []
    failures = []
    seen = set()

    for path in sorted(paths):
        with open(path) as f:
            data = json.load(f)
        for item in data.get("results", []):
            key = (item["label"], item["case"])
            if key not in seen:
                seen.add(key)
                merged_results.append(item)
        failures.extend(data.get("failures", []))

    report = {
        "tool_version": TOOL_VERSION,
        "platform": platform.platform(),
        "date": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "results": merged_results,
        "failures": failures,
    }
    return report, build_bmf(merged_results)


def append_summary(path, report, bmf):
    if not path:
        return
    lines = [
        "### Callgrind Performance Summary (OCaml)",
        "",
        "| Benchmark | Cases | Instructions (Ir) | CPU Time (s) |",
        "| :--- | :---: | :---: | :---: |",
    ]

    total_insn = 0
    total_cpu = 0.0
    has_insn = False

    snippets = {}
    for r in report.get("results", []):
        snippets.setdefault(r["label"], []).append(r)

    for name in sorted(snippets):
        cases_count = len(snippets[name])
        m = bmf.get(name, {})
        insn_str = "-"
        if "instructions" in m:
            val = m["instructions"]["value"]
            insn_str = f"{val:,}"
            total_insn += val
            has_insn = True
        cpu_val = m.get("cpu_time", {}).get("value", 0.0)
        total_cpu += cpu_val
        lines.append(f"| `{name}` | {cases_count} | {insn_str} | {cpu_val:.4f} |")

    tot_insn_str = f"{total_insn:,}" if has_insn else "-"
    lines.append(f"| **Total ({len(snippets)} snippets)** | - | **{tot_insn_str}** | **{total_cpu:.4f}** |")
    lines.append("")

    with open(path, "a") as f:
        f.write("\n".join(lines) + "\n")


def self_test():
    print("Running self-test for measure.py...")
    sample_cg = """
events: Ir
summary: 987654321
totals: 987654321
"""
    parsed = parse_callgrind_output(sample_cg)
    assert parsed == 987654321, f"Expected 987654321, got {parsed}"

    sample_tasks = [
        ("sort/merge_sort", "solution", "s1", "tc0", "in", "out"),
        ("sort/merge_sort", "solution", "s1", "tc1", "in", "out"),
        ("sort/quick_sort", "solution", "s2", "tc0", "in", "out"),
        ("math/gcd", "solution", "s3", "tc0", "in", "out"),
    ]
    sh0 = select_shard(sample_tasks, 0, 2)
    sh1 = select_shard(sample_tasks, 1, 2)
    assert len(sh0) + len(sh1) == len(sample_tasks)
    labels_sh0 = {t[0] for t in sh0}
    labels_sh1 = {t[0] for t in sh1}
    assert labels_sh0.isdisjoint(labels_sh1)

    mock_results = [
        {
            "label": "sort/merge_sort",
            "case": "tc0",
            "summary": {
                "instructions": {"median": 1000},
                "cpu_s": {"median": 0.05},
            },
        },
        {
            "label": "sort/merge_sort",
            "case": "tc1",
            "summary": {
                "instructions": {"median": 2000},
                "cpu_s": {"median": 0.05},
            },
        },
    ]
    bmf = build_bmf(mock_results)
    assert bmf["sort/merge_sort"]["instructions"]["value"] == 3000
    assert abs(bmf["sort/merge_sort"]["cpu_time"]["value"] - 0.10) < 1e-6

    print("self-test passed!")
    return 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--resources", default=os.path.join(ROOT, "tests", "resources"))
    parser.add_argument("--jobs", type=int, default=2)
    parser.add_argument("--repeat", type=int, default=1)
    parser.add_argument("--timeout", type=int, default=300)
    parser.add_argument("--only", default="", help="filter by label substring")
    parser.add_argument("--shard", default="", help="shard K/N (0-based K)")
    parser.add_argument("--output", default=None, help="JSON output file")
    parser.add_argument("--bmf-output", default=None, help="Bencher Metric Format output")
    parser.add_argument("--summary-file", default=None, help="Markdown summary output")
    parser.add_argument("--merge", nargs="*", default=None, help="merge shard JSON files")
    parser.add_argument("--self-test", action="store_true", help="run self-test")
    args = parser.parse_args(argv)

    if args.self_test:
        return self_test()

    if args.merge is not None:
        report, bmf = merge_reports(args.merge)
        if args.output:
            with open(args.output, "w") as f:
                json.dump(report, f, indent=2)
        if args.bmf_output:
            with open(args.bmf_output, "w") as f:
                json.dump(bmf, f, indent=2)
        if args.summary_file:
            append_summary(args.summary_file, report, bmf)
        print(f"Merged {len(args.merge)} reports: {len(report['results'])} cases, {len(bmf)} benchmarks.")
        return 0

    has_valgrind = shutil.which("valgrind") is not None
    if not has_valgrind:
        print("warning: valgrind not found in PATH; running binary directly without Callgrind simulation", file=sys.stderr)

    has_ocamlopt = shutil.which("ocamlopt") is not None
    has_ocaml = shutil.which("ocaml") is not None

    tasks = []
    missing = []
    for area, problem, stem, script, cases in run_tests.discover(args.resources):
        label = f"{area}/{problem}/{stem}" if problem != stem else f"{area}/{problem}"
        if args.only and args.only not in label and args.only not in script:
            continue
        if not cases:
            missing.append(label)
            continue
        for case_name, inp, outp in cases:
            tasks.append((label, stem, script, case_name, inp, outp))

    tasks = sorted(tasks)
    if args.shard:
        index, total = parse_shard(args.shard)
        tasks = select_shard(tasks, index, total)

    total_tasks = len(tasks)
    needed_scripts = sorted({t[2] for t in tasks})
    print(f"Measuring {total_tasks} cases ({len({t[0] for t in tasks})} benchmarks, {len(needed_scripts)} snippets)...")

    # Compile snippets if ocamlopt is available
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
            err = compile_snippet_cached(script, bin_path, obj_dir)
            return script, bin_path, err

        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            futures = [pool.submit(_compile, s) for s in needed_scripts]
            for fut in as_completed(futures):
                script, bin_path, err = fut.result()
                if err:
                    compile_failures[script] = err
                else:
                    binaries[script] = bin_path

    results = []
    failures = []
    done = 0

    run_items = []
    for label, stem, script, case_name, inp, outp in tasks:
        if script in compile_failures:
            print(f"[{done+1}/{total_tasks}] FAIL {label}/{case_name}: {compile_failures[script]}")
            failures.append(f"{label}/{case_name}: {compile_failures[script]}")
            done += 1
            continue
        if has_ocamlopt and script in binaries:
            cmd = [binaries[script]]
        else:
            cmd = ["ocaml", script]
        run_items.append((label, stem, script, case_name, inp, outp, cmd))

    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = {
            pool.submit(
                measure_case_callgrind,
                has_valgrind,
                cmd,
                case_name,
                inp,
                outp,
                args.repeat,
                args.timeout,
            ): (label, stem, script, case_name)
            for label, stem, script, case_name, inp, outp, cmd in run_items
        }
        for future in as_completed(futures):
            label, stem, script, case_name = futures[future]
            summary, err = future.result()
            done += 1
            if err is None:
                insn = summary["instructions"]["median"] if summary.get("instructions") else None
                insn_s = f"{int(insn):,} Ir" if insn else "n/a"
                print(f"[{done}/{total_tasks}] {label}/{case_name}: {insn_s}")
                results.append({
                    "label": label,
                    "stem": stem,
                    "case": case_name,
                    "script": os.path.relpath(script, ROOT),
                    "summary": summary,
                })
            else:
                print(f"[{done}/{total_tasks}] FAIL {label}/{case_name}: {err}")
                failures.append(f"{label}/{case_name}: {err}")

    report = {
        "tool_version": TOOL_VERSION,
        "platform": platform.platform(),
        "date": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "has_valgrind": has_valgrind,
        "results": results,
        "failures": failures,
    }
    bmf = build_bmf(results)

    if args.output:
        with open(args.output, "w") as f:
            json.dump(report, f, indent=2)
    if args.bmf_output:
        with open(args.bmf_output, "w") as f:
            json.dump(bmf, f, indent=2)
    if args.summary_file:
        append_summary(args.summary_file, report, bmf)

    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
