# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The Eilenberg-MacLane Machine (EMM) computes integral (co)homology groups of 1-connected Eilenberg-MacLane spaces K(Z/2^s, n). It is a Haskell program based on the results of Alain Clement's PhD thesis (*Integral Cohomology of Finite Postnikov Towers*, Appendix B).

The original C++ sources (version 2.1, 2002) are preserved in `legacy/` for reference.

## Version Control

This repository uses **Jujutsu (jj)** alongside git. Always use `jj` commands instead of `git` directly.

## Build

```bash
cabal build        # build library + executable
cabal test         # run test suite (hspec + QuickCheck)
cabal run emm      # interactive mode
cabal run emm -- [s] [n] [range]  # CLI mode, LaTeX to stdout
```

## Architecture

```
app/Main.hs                  -- entry point (interactive + CLI modes)
src/EMM/
  Types.hs                   -- Group, GradedGroup (IntMap-based)
  GradedGroups.hs            -- tensor, Tor, Hom, Ext, Kunneth, UCT
  AdmissibleSequences.hs     -- recursive generation, filtering
  Words.hs                   -- Steenrod operation words, g-function
  ElementaryComplex.hs       -- homology of elementary complexes
  EilenbergMacLane.hs        -- main EMHomology algorithm
  LaTeX.hs                   -- LaTeX document rendering
test/
  Spec.hs                    -- test runner
  EMM/*Spec.hs               -- per-module tests
legacy/
  *.cpp, *.hpp               -- original C++ implementation
```

## Running

- **Interactive mode**: `cabal run emm` -- prompts for parameters s, n, range
- **CLI mode**: `cabal run emm -- [s] [n] [range]` -- outputs LaTeX to stdout
- Interactive mode also writes results to `output.tex`

Parameters: `s` = log-order of cyclic group Z/2^s, `n` = connectivity+1, `range` = upper bound for computation.

## Docker

```bash
docker build -t aclemen1/emm .
docker run -it --rm aclemen1/emm
```
