#!/usr/bin/env python3
"""Analyze GEMM benchmark results: log-log regression for complexity exponents.

Usage:
    cabal bench --benchmark-options='--csv bench/results.csv'
    uv run python bench/analyze.py bench/results.csv

Reads the CSV produced by tasty-bench and fits T ~ c * R^alpha via
log-log least squares for each benchmark group.
"""

import csv
import re
import sys
from collections import defaultdict
from math import log


def parse_results(csv_path: str) -> dict[str, list[tuple[int, float]]]:
    """Parse tasty-bench CSV into {group: [(R, time_seconds), ...]}."""
    groups: dict[str, list[tuple[int, float]]] = defaultdict(list)

    with open(csv_path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row["Name"]
            # Extract group and R from name like "group/K(...) R=400"
            match = re.search(r"R=(\d+)", name)
            if not match:
                continue
            r = int(match.group(1))
            # Group name: tasty-bench uses "All.group.bench" format
            parts = name.split(".")
            group = parts[1] if len(parts) >= 3 else name
            # Time is in picoseconds
            time_ps = float(row["Mean (ps)"])
            time_s = time_ps * 1e-12
            groups[group].append((r, time_s))

    return dict(groups)


def log_log_regression(
    points: list[tuple[int, float]],
) -> tuple[float, float]:
    """Fit log(T) = alpha * log(R) + log(c) via least squares.

    Returns (alpha, c).
    """
    n = len(points)
    if n < 2:
        return (0.0, 0.0)

    sum_x = sum_y = sum_xx = sum_xy = 0.0
    for r, t in points:
        x = log(r)
        y = log(t)
        sum_x += x
        sum_y += y
        sum_xx += x * x
        sum_xy += x * y

    denom = n * sum_xx - sum_x * sum_x
    if abs(denom) < 1e-15:
        return (0.0, 0.0)

    alpha = (n * sum_xy - sum_x * sum_y) / denom
    log_c = (sum_y - alpha * sum_x) / n
    from math import exp

    return (alpha, exp(log_c))


# Theoretical exponents for each configuration
THEORETICAL = {
    # beta = 1
    "p=2,f=1,n=2 (beta=1)": 3.0,
    "p=3,f=1,n=2 (beta=1)": 3.0,
    "p=5,f=1,n=2 (beta=1)": 3.0,
    "p=7,f=1,n=2 (beta=1)": 3.0,
    "p=3,f=1,n=3 (beta=1)": 3.0,
    # f > 1, beta = 1
    "p=2,f=2,n=2 (beta=1)": 3.0,
    "p=2,f=3,n=2 (beta=1)": 3.0,
    "p=3,f=2,n=2 (beta=1)": 3.0,
    # beta = 2
    "p=2,f=1,n=3 (beta=2)": 4.0,
    "p=2,f=1,n=4 (beta=2)": 4.0,
    "p=3,f=1,n=4 (beta=2)": 3.631,
    "p=2,f=2,n=3 (beta=2)": 4.0,
    # beta = 3
    "p=2,f=1,n=5 (beta=3)": 4.585,
    # beta = 4
    "p=2,f=1,n=7 (beta=4)": 5.0,
    # K(Z, n)
    "K(Z,2)": 1.0,
    "K(Z,3)": 4.0,
    "K(Z,4)": 4.0,
}


def main() -> None:
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <results.csv>", file=sys.stderr)
        sys.exit(1)

    csv_path = sys.argv[1]
    groups = parse_results(csv_path)

    if not groups:
        print("No benchmark data found.", file=sys.stderr)
        sys.exit(1)

    # Print LaTeX table
    print("% ── Empirical complexity exponents ──")
    print(r"\begin{tabular}{lcccc}")
    print(r"\hline")
    print(
        r"Configuration & $\beta$ & $\alpha_{\text{th}}$ "
        r"& $\alpha_{\text{emp}}$ & $R$ range \\"
    )
    print(r"\hline")

    # Also write pgfplots data files
    for group_name, points in sorted(groups.items()):
        points.sort()
        alpha_emp, c = log_log_regression(points)
        alpha_th = THEORETICAL.get(group_name, "?")
        r_min, r_max = points[0][0], points[-1][0]

        # Extract beta from group name (may be β or beta)
        beta_match = re.search(r"(?:β|beta)=(\d+)", group_name)
        beta = beta_match.group(1) if beta_match else "?"

        print(
            f"  {group_name} & ${beta}$ & ${alpha_th}$ "
            f"& ${alpha_emp:.2f}$ & ${r_min}$--${r_max}$ \\\\"
        )

        # Write data file for pgfplots
        safe_name = re.sub(r"[^a-zA-Z0-9]", "_", group_name)
        data_path = f"bench/{safe_name}.dat"
        with open(data_path, "w") as f:
            f.write("# R  time_s\n")
            for r, t in points:
                f.write(f"{r}  {t:.6e}\n")

    print(r"\hline")
    print(r"\end{tabular}")


if __name__ == "__main__":
    main()
