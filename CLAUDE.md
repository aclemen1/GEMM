# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The Generalized Eilenberg-MacLane Machine (GEMM) computes integral (co)homology groups of Eilenberg-MacLane spaces K(Z/p^f, n) for any prime p, and K(Z, n). It is a Haskell program based on Cartan's séminaire (1954–55) and the results of Alain Clement's PhD thesis (*Integral Cohomology of Finite Postnikov Towers*, Appendix B).

The original C++ sources (version 2.1, 2002) are preserved in `legacy/` for reference.
The companion paper (algorithm description and conjectures) is in `paper/`.

## Version Control

This repository uses **Jujutsu (jj)** alongside git. Always use `jj` commands instead of `git` directly.

## Build

```bash
cabal build         # build library + executable
cabal test          # run test suite (hspec + QuickCheck)
cabal run gemm      # interactive mode
cabal run gemm -- [p] [f] [n] [range]     # CLI mode, LaTeX to stdout
cabal run gemm -- --json [p] [f] [n] [range]  # CLI mode, JSON to stdout
cabal run gemm -- Z [n] [range]            # K(Z, n) mode
```

## Architecture

```
app/Main.hs                  -- entry point (interactive + CLI modes)
src/GEMM/
  Types.hs                   -- Group, GradedGroup (Map-based)
  GradedGroups.hs            -- tensor, Tor, Hom, Ext, Kunneth, UCT
  AdmissibleSequences.hs     -- recursive generation, filtering
  Words.hs                   -- Steenrod operation words, g-function
  ElementaryComplex.hs       -- homology of elementary complexes
  EilenbergMacLane.hs        -- main algorithm (any prime p, and K(Z,n))
  LaTeX.hs                   -- LaTeX document rendering
  JSON.hs                    -- JSON output rendering
test/
  Spec.hs                    -- test runner
  GEMM/*Spec.hs              -- per-module tests
legacy/
  *.cpp, *.hpp               -- original C++ implementation
paper/
  algorithm.tex              -- companion paper (algorithm + conjectures)
```

## Running

- **Interactive mode**: `cabal run gemm` -- prompts for parameters p, f, n, range
- **CLI mode**: `cabal run gemm -- [p] [f] [n] [range]` -- outputs LaTeX to stdout
- **JSON mode**: `cabal run gemm -- --json [p] [f] [n] [range]` -- outputs JSON to stdout
- **K(Z, n)**: `cabal run gemm -- [--json] Z [n] [range]`
- Backward compatible: `cabal run gemm -- [s] [n] [range]` (3 args → p=2, f=s)
- Interactive mode also writes results to `output.tex`

Parameters: `p` = prime, `f` = log-order of cyclic group Z/p^f, `n` = connectivity+1, `range` = upper bound for computation.

## Docker

```bash
docker build -t aclemen1/gemm .
docker run -it --rm aclemen1/gemm
```
