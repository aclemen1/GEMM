#!/usr/bin/env python3
"""Complexity decomposition tree for the GEMM algorithm.

Usage:
    uv run --with matplotlib python bench/plot_complexity_tree.py

Produces a visual breakdown of how O(R^{2+log_p(beta)} * C^2) is derived.
"""

import os
os.environ["PATH"] = "/usr/local/texlive/2026/bin/universal-darwin:" + os.environ.get("PATH", "")

import matplotlib
matplotlib.use("Agg")
matplotlib.rcParams["text.usetex"] = True
matplotlib.rcParams["text.latex.preamble"] = r"\usepackage{amsmath}"
matplotlib.rcParams["font.family"] = "serif"
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch


# ── Layout constants ─────────────────────────────────────────────────

NODE_W = 3.4      # node width
NODE_H = 0.95     # node height
VSTEP = 1.55      # vertical step between levels
XMID = 0.0        # center x


def box(ax, x, y, title, subtitle, color, w=NODE_W, h=NODE_H):
    """Draw a rounded box node."""
    p = FancyBboxPatch(
        (x - w/2, y - h/2), w, h,
        boxstyle="round,pad=0.10", lw=1.6,
        ec=color, fc=color + "14", zorder=3)
    ax.add_patch(p)
    ax.text(x, y + 0.15, title, ha="center", va="center",
            fontsize=8.5, fontweight="bold", zorder=4)
    ax.text(x, y - 0.18, subtitle, ha="center", va="center",
            fontsize=7.5, style="italic", color="#555555", zorder=4)
    return (x, y)


def arrow(ax, x1, y1, x2, y2):
    """Simple arrow between nodes."""
    ax.annotate("", xy=(x2, y2 + NODE_H/2),
                xytext=(x1, y1 - NODE_H/2),
                arrowprops=dict(arrowstyle="-|>", color="#999999",
                                lw=1.2, shrinkA=1, shrinkB=1),
                zorder=2)


def factor_label(ax, x1, y1, x2, y2, text, side="right"):
    """Multiplication factor label next to an arrow."""
    mx = (x1 + x2) / 2
    my = (y1 + y2) / 2
    dx = 0.15 if side == "right" else -0.15
    ha = "left" if side == "right" else "right"
    ax.text(mx + dx, my, text, ha=ha, va="center", fontsize=7.5,
            color="#b83030", fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.15", fc="#fff6f0",
                      ec="#e8c8a8", lw=0.7),
            zorder=5)


def mult_sign(ax, x, y):
    """Draw a × sign."""
    ax.text(x, y, r"$\times$", ha="center", va="center",
            fontsize=14, color="#b83030", fontweight="bold", zorder=5)


def main():
    fig, ax = plt.subplots(figsize=(10, 11.5))
    ax.set_xlim(-5.5, 5.5)
    ax.set_ylim(-11.5, 1.8)
    ax.set_aspect("equal")
    ax.axis("off")

    # Colors
    CB = "#2166ac"   # blue  (loops)
    CR = "#b2182b"   # red   (recursion)
    CG = "#1b7837"   # green (leaf ops)
    CP = "#762a83"   # purple (result)

    # ── Title ──
    ax.text(XMID, 1.4, "GEMM — Complexity Decomposition Tree",
            ha="center", fontsize=14, fontweight="bold")
    ax.text(XMID, 0.85,
            r"Each level multiplies its factor $\rightarrow$"
            r" product = total cost",
            ha="center", fontsize=9.5, color="#666666")

    # ══════════════════════════════════════════════════════════════════
    #  LEFT COLUMN: Algorithm tree       RIGHT COLUMN: Cost factors
    # ══════════════════════════════════════════════════════════════════

    tree_x = -1.0
    cost_x = 3.8
    y0 = 0.0

    # ── Level 0: Main loop ──
    y = y0
    box(ax, tree_x, y, "Algorithm 6: Main loop",
        r"for each stable degree $s = 0 \ldots D$", CB)
    # Cost factor
    ax.text(cost_x, y, r"$D + 1$ iterations", ha="center", va="center",
            fontsize=8, color=CB, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.2", fc="#e8f0fa", ec=CB, lw=1.0))
    ax.text(cost_x, y - 0.38,
            r"$D = \left\lfloor\frac{R-n}{p-1}\right\rfloor$",
            ha="center", fontsize=7.5, color="#666666")

    # ── Level 1: Recursion ──
    y1 = y - VSTEP
    arrow(ax, tree_x, y, tree_x, y1)
    box(ax, tree_x, y1, "Algorithms 3–4: Recursion tree",
        r"branching $\beta$, depth $\leq \log_p s$", CR, w=3.6)
    ax.text(cost_x, y1,
            r"$N(s) \leq \beta^{\log_p s}$ leaves",
            ha="center", va="center", fontsize=8, color=CR, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.2", fc="#fce8e8", ec=CR, lw=1.0))
    ax.text(cost_x, y1 - 0.38,
            r"$\sum_s N(s) = S = O(D^{1+\log_p\beta})$",
            ha="center", fontsize=7.5, color="#666666")

    # Multiply sign between levels
    mult_sign(ax, cost_x + 2.2, (y + y1) / 2)

    # ── Level 2: Per sequence → Künneth ──
    y2 = y1 - VSTEP
    arrow(ax, tree_x, y1, tree_x, y2)
    box(ax, tree_x, y2, "Algorithm 5: Künneth product",
        r"$H \leftarrow H \otimes_K H_*(E_k)$  for each sequence", CB)
    ax.text(cost_x, y2, r"$1$ per sequence", ha="center", va="center",
            fontsize=8, color=CB, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.2", fc="#e8f0fa", ec=CB, lw=1.0))

    mult_sign(ax, cost_x + 2.2, (y1 + y2) / 2)

    # ── Level 3: Output degree loop ──
    y3 = y2 - VSTEP
    arrow(ax, tree_x, y2, tree_x, y3)
    box(ax, tree_x, y3, "Output degree loop",
        r"for $m = 0, 1, \ldots, R$", CB)
    ax.text(cost_x, y3, r"$R + 1$ iterations", ha="center", va="center",
            fontsize=8, color=CB, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.2", fc="#e8f0fa", ec=CB, lw=1.0))

    mult_sign(ax, cost_x + 2.2, (y2 + y3) / 2)

    # ── Level 4: Sparse inner loop ──
    y4 = y3 - VSTEP
    arrow(ax, tree_x, y3, tree_x, y4)
    box(ax, tree_x, y4, "Sparse inner loop",
        r"for $j \in \mathrm{supp}(E_k)$", CB, w=3.2)
    ax.text(cost_x, y4,
            r"$|\mathrm{supp}(E_k)| = R/\delta_k$ pairs",
            ha="center", va="center", fontsize=8, color=CB, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.2", fc="#e8f0fa", ec=CB, lw=1.0))
    ax.text(cost_x, y4 - 0.38,
            r"$\delta_k = n + (p{-}1)\,s_k$ (Lemma 7.6)",
            ha="center", fontsize=7.5, color="#666666")

    mult_sign(ax, cost_x + 2.2, (y3 + y4) / 2)

    # ── Level 5: Bilinear operations ──
    y5 = y4 - VSTEP
    arrow(ax, tree_x, y4, tree_x, y5)
    box(ax, tree_x, y5,
        r"$\otimes$ and Tor on Group pairs",
        r"bilinear on cyclic components", CG)
    ax.text(cost_x, y5, r"$O(C^2)$ per pair", ha="center", va="center",
            fontsize=8, color=CG, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.2", fc="#e8f8e8", ec=CG, lw=1.0))
    ax.text(cost_x, y5 - 0.38,
            r"$C = f + \lfloor\log_p R\rfloor + 1$ (Cor. 7.4)",
            ha="center", fontsize=7.5, color="#666666")

    mult_sign(ax, cost_x + 2.2, (y4 + y5) / 2)

    # ── Aggregation annotation ──
    # Show how the sum over s absorbs the 1/delta_k
    agg_y = (y1 + y4) / 2
    agg_x = -4.5
    ax.text(agg_x, agg_y,
            r"$\underbrace{\sum_{s=0}^{D}"
            r"\frac{N(s)}{\delta(s)}}_{"
            r"\textrm{levels 0--1--4}}$"
            "\n\n"
            r"$= O\!\left(\frac{D^{\log_p\beta}}{p-1}\right)$",
            ha="center", va="center", fontsize=8.5, color="#555555",
            bbox=dict(boxstyle="round,pad=0.3", fc="#f8f8ff",
                      ec="#ccccdd", lw=0.8, linestyle="--"))
    # Arrow from this box to the tree
    ax.annotate("", xy=(tree_x - NODE_W/2, y1),
                xytext=(agg_x + 1.1, agg_y + 0.4),
                arrowprops=dict(arrowstyle="-|>", color="#aaaacc",
                                lw=1.0, linestyle="--"),
                zorder=1)
    ax.annotate("", xy=(tree_x - NODE_W/2 - 0.1, y4),
                xytext=(agg_x + 1.1, agg_y - 0.6),
                arrowprops=dict(arrowstyle="-|>", color="#aaaacc",
                                lw=1.0, linestyle="--"),
                zorder=1)

    # ══════════════════════════════════════════════════════════════════
    #  BOTTOM: Final result
    # ══════════════════════════════════════════════════════════════════

    yr = y5 - 1.6
    result = FancyBboxPatch(
        (-4.0, yr - 0.55), 8.0, 1.1,
        boxstyle="round,pad=0.12", lw=2.2,
        ec=CP, fc="#f8f0ff", zorder=3)
    ax.add_patch(result)

    ax.text(XMID, yr + 0.18,
            r"$\mathrm{Total} = "
            r"\underbrace{\frac{D^{\log_p\beta}}{p-1}}_{"
            r"\text{sequences}/\delta}"
            r"\;\times\; \underbrace{R\vphantom{D^{\beta}}}_{"
            r"\text{degrees}}"
            r"\;\times\; \underbrace{C^2\vphantom{D^{\beta}}}_{"
            r"\text{pairs}}"
            r"\;=\; "
            r"\frac{R^{2+\log_p\beta}}{(p-1)^{1+\log_p\beta}}"
            r"\cdot (f + \log_p R)^2$",
            ha="center", va="center", fontsize=10,
            color=CP, fontweight="bold", zorder=4)

    ax.text(XMID, yr - 0.30,
            r"For fixed $p, f, n$:  $\boxed{\,O\!\left("
            r"R^{2+\log_p\beta} \cdot \log^2 R"
            r"\right)\,}$ --- Theorem 7.5 (sparse K\"{u}nneth)",
            ha="center", va="center", fontsize=9, color="#555555", zorder=4)

    fig.tight_layout()
    out = "bench/complexity_tree.pdf"
    fig.savefig(out, dpi=200, bbox_inches="tight")
    print(f"Saved to {out}")


if __name__ == "__main__":
    main()
