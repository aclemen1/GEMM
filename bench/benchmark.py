#!/usr/bin/env python3
"""Empirical complexity benchmark for GEMM.

Measures wall-clock time and peak memory for each configuration at
increasing ranges R, then computes doubling ratios to estimate the
empirical complexity exponent.

Usage:
    cd GEMM
    cabal build gemm
    uv run python bench/benchmark.py

Output:
    bench/benchmark_results.csv   — raw measurements
    stdout                        — summary table with doubling ratios

This script is agnostic of any theoretical complexity bound: it only
measures and reports empirical data.
"""

import csv
import os
import re
import subprocess
import sys
from math import log2

# ── Configurations to benchmark ─────────────────────────────────────
# Each entry: (label, cli_args, ranges)
#   cli_args are passed to `cabal run gemm -- +RTS -s -RTS <cli_args> <range>`
#   ranges: adapted per config so that the largest R runs in < 2 min
CONFIGS = [
    # ── beta = 1 ──
    # p=2: moderate speed
    ("p=2,f=1,n=2", ["2", "1", "2"],
     [500, 1000, 2000, 4000, 8000, 16000]),
    # p>=3, n=2: very fast — need huge R
    ("p=3,f=1,n=2", ["3", "1", "2"],
     [4000, 8000, 16000, 32000, 64000, 128000]),
    ("p=5,f=1,n=2", ["5", "1", "2"],
     [4000, 8000, 16000, 32000, 64000, 128000]),
    ("p=7,f=1,n=2", ["7", "1", "2"],
     [4000, 8000, 16000, 32000, 64000, 128000]),
    ("p=2,f=2,n=2", ["2", "2", "2"],
     [500, 1000, 2000, 4000, 8000, 16000]),
    # p>=3, n=3: moderate
    ("p=3,f=1,n=3", ["3", "1", "3"],
     [500, 1000, 2000, 4000, 8000, 16000]),
    ("p=5,f=1,n=3", ["5", "1", "3"],
     [500, 1000, 2000, 4000, 8000, 16000]),
    ("p=7,f=1,n=3", ["7", "1", "3"],
     [500, 1000, 2000, 4000, 8000, 16000]),
    # ── beta = 2 ──
    ("p=2,f=1,n=3", ["2", "1", "3"],
     [500, 1000, 2000, 4000, 8000]),
    ("p=2,f=1,n=4", ["2", "1", "4"],
     [200, 400, 800, 1600, 3200]),
    ("p=3,f=1,n=4", ["3", "1", "4"],
     [500, 1000, 2000, 4000, 8000]),
    ("p=5,f=1,n=4", ["5", "1", "4"],
     [500, 1000, 2000, 4000, 8000]),
    ("p=2,f=2,n=3", ["2", "2", "3"],
     [500, 1000, 2000, 4000, 8000]),
    # ── beta = 3 ──
    ("p=2,f=1,n=5", ["2", "1", "5"],
     [200, 400, 800, 1600, 3200]),
    ("p=3,f=1,n=5", ["3", "1", "5"],
     [200, 400, 800, 1600, 3200]),
    # ── beta = 4 ──
    ("p=2,f=1,n=7", ["2", "1", "7"],
     [100, 200, 400, 800, 1600]),
    # ── K(Z, n) ──
    ("K(Z,2)", ["Z", "2"],
     [4000, 8000, 16000, 32000, 64000, 128000]),
    ("K(Z,3)", ["Z", "3"],
     [500, 1000, 2000, 4000, 8000, 16000]),
    ("K(Z,4)", ["Z", "4"],
     [500, 1000, 2000, 4000, 8000]),
]

# Maximum wall-clock time per run (seconds) — skip larger ranges if exceeded
TIMEOUT = 300


def find_gemm():
    """Locate the gemm executable via cabal."""
    result = subprocess.run(
        ["cabal", "list-bin", "gemm"],
        capture_output=True, text=True,
    )
    if result.returncode != 0:
        print("Error: cannot find gemm executable. Run 'cabal build gemm' first.",
              file=sys.stderr)
        sys.exit(1)
    return result.stdout.strip()


def parse_rts_stats(stderr_text):
    """Extract peak memory (bytes) and elapsed time (seconds) from GHC RTS output."""
    # "56,120 bytes maximum residency (1 sample(s))" — precise byte count
    res_match = re.search(
        r"([\d,]+)\s+bytes maximum residency",
        stderr_text,
    )
    # Fallback: "6 MiB total memory in use" — coarser
    tot_match = re.search(
        r"(\d[\d,]*)\s+(bytes|[KkMmGg]i?[Bb])\s+total memory in use",
        stderr_text,
    )

    peak_mem_bytes = 0
    if res_match:
        peak_mem_bytes = int(res_match.group(1).replace(",", ""))
    elif tot_match:
        val = int(tot_match.group(1).replace(",", ""))
        unit = tot_match.group(2).lower()
        if "gib" in unit or "gb" in unit:
            peak_mem_bytes = val * 1024 * 1024 * 1024
        elif "mib" in unit or "mb" in unit:
            peak_mem_bytes = val * 1024 * 1024
        elif "kib" in unit or "kb" in unit:
            peak_mem_bytes = val * 1024
        else:
            peak_mem_bytes = val

    # Heap allocation: "12,404,160 bytes allocated in the heap"
    alloc_match = re.search(
        r"([\d,]+)\s+bytes allocated in the heap",
        stderr_text,
    )
    heap_alloc = int(alloc_match.group(1).replace(",", "")) if alloc_match else 0

    # "Total   time    0.004s  (  0.013s elapsed)"
    time_match = re.search(
        r"Total\s+time\s+[\d.]+s\s+\(\s*([\d.]+)s\s+elapsed\)",
        stderr_text,
    )
    elapsed_s = float(time_match.group(1)) if time_match else 0.0

    return elapsed_s, peak_mem_bytes, heap_alloc


def parse_gemm_time(stderr_text):
    """Extract GEMM's own --time report (homology CPU time)."""
    # Try seconds first: "homology: 2.107 s"
    match = re.search(r"homology:\s*([\d.]+)\s*s\b", stderr_text)
    if match:
        return float(match.group(1))
    # Then milliseconds: "homology: 1.2 ms"
    match = re.search(r"homology:\s*([\d.]+)\s*ms", stderr_text)
    if match:
        return float(match.group(1)) * 1e-3
    return None


def run_benchmark(gemm_bin, cli_args, R):
    """Run gemm for given config and range R.

    Returns (elapsed_s, peak_residency_bytes, heap_alloc_bytes, cpu_time_s).
    """
    cmd = [gemm_bin, "+RTS", "-s", "-RTS", "--time"] + cli_args + [str(R)]
    try:
        result = subprocess.run(
            cmd,
            capture_output=True, text=True,
            timeout=TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        return None, None, None, None

    if result.returncode != 0:
        return None, None, None, None

    elapsed_s, peak_mem, heap_alloc = parse_rts_stats(result.stderr)
    cpu_time = parse_gemm_time(result.stderr)
    return elapsed_s, peak_mem, heap_alloc, cpu_time


def fmt_mem(b):
    """Format bytes as human-readable."""
    if b is None:
        return "—"
    if b >= 1024 * 1024 * 1024:
        return f"{b / (1024**3):.1f} GiB"
    if b >= 1024 * 1024:
        return f"{b / (1024**2):.1f} MiB"
    if b >= 1024:
        return f"{b / 1024:.1f} KiB"
    return f"{b} B"


def doubling_ratio(prev, curr):
    """Compute doubling ratio curr/prev, or None."""
    if prev is None or curr is None or prev <= 0:
        return None
    return curr / prev


def log2_ratio(ratio):
    """Compute log2 of a doubling ratio (= empirical exponent)."""
    if ratio is None or ratio <= 0:
        return None
    return log2(ratio)


def main():
    gemm_bin = find_gemm()
    print(f"Using: {gemm_bin}")
    print()

    bench_dir = os.path.dirname(os.path.abspath(__file__))
    csv_path = os.path.join(bench_dir, "benchmark_results.csv")

    # Header
    hdr = (
        f"{'Config':<20} {'R':>5} {'CPU (s)':>10} {'Alloc':>12} "
        f"{'Resid':>10} {'T ratio':>8} {'M ratio':>8} {'α_T':>6} {'α_M':>6}"
    )
    print(hdr)
    print("─" * len(hdr))

    with open(csv_path, "w", newline="") as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow([
            "config", "R",
            "cpu_time_s", "elapsed_s",
            "heap_alloc_bytes", "peak_residency_bytes",
            "time_doubling_ratio", "alloc_doubling_ratio",
            "alpha_time", "alpha_alloc",
        ])

        for label, cli_args, ranges in CONFIGS:
            prev_time = None
            prev_alloc = None
            skip_rest = False

            for R in ranges:
                if skip_rest:
                    break

                elapsed, residency, alloc, cpu = run_benchmark(
                    gemm_bin, cli_args, R,
                )

                if elapsed is None:
                    print(f"{label:<20} {R:>5}   (timeout or error)")
                    skip_rest = True
                    continue

                # Prefer GEMM CPU time; fall back to RTS elapsed
                time_val = cpu if cpu and cpu > 0 else elapsed

                t_ratio = doubling_ratio(prev_time, time_val)
                m_ratio = doubling_ratio(prev_alloc, alloc)
                alpha_t = log2_ratio(t_ratio)
                alpha_m = log2_ratio(m_ratio)

                writer.writerow([
                    label, R,
                    f"{cpu:.6f}" if cpu else "",
                    f"{elapsed:.6f}",
                    alloc, residency,
                    f"{t_ratio:.4f}" if t_ratio else "",
                    f"{m_ratio:.4f}" if m_ratio else "",
                    f"{alpha_t:.2f}" if alpha_t else "",
                    f"{alpha_m:.2f}" if alpha_m else "",
                ])

                t_str = f"{time_val:.4f}" if time_val < 10 else f"{time_val:.1f}"
                tr_str = f"{t_ratio:.2f}" if t_ratio else "—"
                mr_str = f"{m_ratio:.2f}" if m_ratio else "—"
                at_str = f"{alpha_t:.2f}" if alpha_t else "—"
                am_str = f"{alpha_m:.2f}" if alpha_m else "—"

                print(
                    f"{label:<20} {R:>5} {t_str:>10} {fmt_mem(alloc):>12} "
                    f"{fmt_mem(residency):>10} {tr_str:>8} {mr_str:>8} "
                    f"{at_str:>6} {am_str:>6}",
                    flush=True,
                )

                prev_time = time_val
                prev_alloc = alloc

            print()  # blank line between configs

    print(f"Results written to {csv_path}")
    print()
    print("Legend:")
    print("  CPU (s)  = GEMM homology computation time")
    print("  Alloc    = total heap allocation (bytes)")
    print("  Resid    = peak memory residency (bytes)")
    print("  T ratio  = T(2R)/T(R)    — time doubling ratio")
    print("  M ratio  = A(2R)/A(R)    — allocation doubling ratio")
    print("  α_T      = log₂(T ratio) — empirical time exponent")
    print("  α_M      = log₂(M ratio) — empirical space exponent")


if __name__ == "__main__":
    main()
