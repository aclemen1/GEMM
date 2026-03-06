#!/usr/bin/env python3
"""Plot empirical complexity from GEMM benchmark results.

Usage:
    cabal bench --benchmark-options='--csv bench/results.csv'
    uv run --with matplotlib python bench/plot_complexity.py bench/results.csv

Produces a log-log plot of wall-clock time vs R for each benchmark group,
with theoretical slope lines for the naive and sparse bounds.
"""

import csv
import re
import sys
from collections import defaultdict
from math import log, log2, floor, exp

# ── Configuration ────────────────────────────────────────────────────

# Theoretical exponents: (naive upper, sparse upper)
# naive = 3 + log_p(beta), sparse = 2 + log_p(beta)
CONFIGS = {
    # beta = 1
    "p=2,f=1,n=2 (beta=1)":  (2, 1, 2, 1),  # (p, f, n, beta)
    "p=3,f=1,n=2 (beta=1)":  (3, 1, 2, 1),
    "p=5,f=1,n=2 (beta=1)":  (5, 1, 2, 1),
    "p=7,f=1,n=2 (beta=1)":  (7, 1, 2, 1),
    "p=3,f=1,n=3 (beta=1)":  (3, 1, 3, 1),
    "p=2,f=2,n=2 (beta=1)":  (2, 2, 2, 1),
    "p=2,f=3,n=2 (beta=1)":  (2, 3, 2, 1),
    "p=3,f=2,n=2 (beta=1)":  (3, 2, 2, 1),
    # beta = 2
    "p=2,f=1,n=3 (beta=2)":  (2, 1, 3, 2),
    "p=2,f=1,n=4 (beta=2)":  (2, 1, 4, 2),
    "p=3,f=1,n=4 (beta=2)":  (3, 1, 4, 2),
    "p=2,f=2,n=3 (beta=2)":  (2, 2, 3, 2),
    # beta = 3
    "p=2,f=1,n=5 (beta=3)":  (2, 1, 5, 3),
    # beta = 4
    "p=2,f=1,n=7 (beta=4)":  (2, 1, 7, 4),
    # K(Z, n)
    "K(Z,2)": (2, 0, 2, 0),  # special: torsion-free
    "K(Z,3)": (2, 0, 3, 2),
    "K(Z,4)": (2, 0, 4, 2),
}


def exponents(p, beta):
    """Return (alpha_naive, alpha_sparse) for given p and beta."""
    if beta <= 0:
        return (1.0, 1.0)  # torsion-free case
    if beta == 1:
        return (3.0, 2.0)
    lb = log(beta) / log(p)
    return (3.0 + lb, 2.0 + lb)


def parse_results(csv_path):
    """Parse tasty-bench CSV into {group: [(R, time_s), ...]}."""
    groups = defaultdict(list)
    with open(csv_path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row["Name"]
            match = re.search(r"R=(\d+)", name)
            if not match:
                continue
            r = int(match.group(1))
            parts = name.split(".")
            group = parts[1] if len(parts) >= 3 else name
            time_ps = float(row["Mean (ps)"])
            time_s = time_ps * 1e-12
            groups[group].append((r, time_s))
    return dict(groups)


def log_log_regression(points):
    """Fit log(T) = alpha * log(R) + log(c). Returns (alpha, c)."""
    n = len(points)
    if n < 2:
        return (0.0, 0.0)
    sum_x = sum_y = sum_xx = sum_xy = 0.0
    for r, t in points:
        x, y = log(r), log(t)
        sum_x += x; sum_y += y; sum_xx += x * x; sum_xy += x * y
    denom = n * sum_xx - sum_x * sum_x
    if abs(denom) < 1e-15:
        return (0.0, 0.0)
    alpha = (n * sum_xy - sum_x * sum_y) / denom
    log_c = (sum_y - alpha * sum_x) / n
    return (alpha, exp(log_c))


def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <results.csv>", file=sys.stderr)
        sys.exit(1)

    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.lines import Line2D

    groups = parse_results(sys.argv[1])
    if not groups:
        print("No benchmark data found.", file=sys.stderr)
        sys.exit(1)

    # ── Select representative configs (one per beta, p=2 preferred) ──
    representatives = [
        ("p=2,f=1,n=2 (beta=1)", r"$K(\mathbb{Z}/2, 2)$, $\beta=1$"),
        ("p=2,f=1,n=3 (beta=2)", r"$K(\mathbb{Z}/2, 3)$, $\beta=2$"),
        ("p=2,f=1,n=5 (beta=3)", r"$K(\mathbb{Z}/2, 5)$, $\beta=3$"),
        ("p=2,f=1,n=7 (beta=4)", r"$K(\mathbb{Z}/2, 7)$, $\beta=4$"),
    ]

    colors = ["#2166ac", "#d6604d", "#4daf4a", "#984ea3"]

    fig, ax = plt.subplots(1, 1, figsize=(8, 5.5))

    for idx, (key, label) in enumerate(representatives):
        if key not in groups:
            continue
        points = sorted(groups[key])
        rs = [r for r, _ in points]
        ts = [t for _, t in points]
        color = colors[idx]

        # Empirical data points
        alpha_emp, c_emp = log_log_regression(points)
        ax.loglog(rs, ts, "o-", color=color, linewidth=1.5, markersize=6,
                  label=f"{label}  ($\\alpha_{{emp}}={alpha_emp:.2f}$)", zorder=3)

        # Theoretical reference lines
        p, f, n, beta = CONFIGS[key]
        alpha_naive, alpha_sparse = exponents(p, beta)

        r_min, r_max = rs[0], rs[-1]
        r_ref = [r_min, r_max]

        # Anchor at empirical midpoint
        r_mid = (r_min * r_max) ** 0.5
        t_mid = c_emp * r_mid ** alpha_emp

        # Naive slope (dashed)
        t_naive = [t_mid * (r / r_mid) ** alpha_naive for r in r_ref]
        ax.loglog(r_ref, t_naive, "--", color=color, alpha=0.4, linewidth=1.0)

        # Sparse slope (dotted)
        t_sparse = [t_mid * (r / r_mid) ** alpha_sparse for r in r_ref]
        ax.loglog(r_ref, t_sparse, ":", color=color, alpha=0.4, linewidth=1.0)

    # ── Legend entries for line styles ──
    custom_lines = [
        Line2D([0], [0], color="gray", linestyle="-", linewidth=1.5, marker="o", markersize=5),
        Line2D([0], [0], color="gray", linestyle="--", linewidth=1.0, alpha=0.5),
        Line2D([0], [0], color="gray", linestyle=":", linewidth=1.0, alpha=0.5),
    ]
    leg1 = ax.legend(loc="upper left", fontsize=8.5, framealpha=0.9)
    leg2 = ax.legend(custom_lines,
                     [r"Empirical ($\alpha_{emp}$)",
                      r"Naive bound ($3 + \log_p\beta$)",
                      r"Sparse bound ($2 + \log_p\beta$)"],
                     loc="lower right", fontsize=8, framealpha=0.9)
    ax.add_artist(leg1)

    ax.set_xlabel("Range $R$", fontsize=11)
    ax.set_ylabel("Wall-clock time (s)", fontsize=11)
    ax.set_title("GEMM empirical complexity (sparse K\u00fcnneth)", fontsize=12)
    ax.grid(True, which="both", alpha=0.2)
    ax.tick_params(labelsize=9)

    fig.tight_layout()
    out_path = "bench/complexity.pdf"
    fig.savefig(out_path, dpi=150)
    print(f"Plot saved to {out_path}")

    # ── Also produce a summary table ──
    print()
    print(f"{'Group':<30} {'beta':>4} {'naive':>6} {'sparse':>7} {'emp':>6} {'R range':>12}")
    print("-" * 72)
    for key, points in sorted(groups.items()):
        points.sort()
        alpha_emp, _ = log_log_regression(points)
        cfg = CONFIGS.get(key)
        if cfg:
            p, f, n, beta = cfg
            alpha_naive, alpha_sparse = exponents(p, beta)
            r_range = f"{points[0][0]}--{points[-1][0]}"
            print(f"{key:<30} {beta:>4} {alpha_naive:>6.2f} {alpha_sparse:>7.2f} "
                  f"{alpha_emp:>6.2f} {r_range:>12}")


if __name__ == "__main__":
    main()
