# The Eilenberg-MacLane Machine (EMM)

The Eilenberg-MacLane Machine (EMM) computes the integral (co)homology groups of 1-connected Eilenberg-MacLane spaces of the form K(Z/2^s, n).

This version is a complete rewrite in Haskell, based on the results of Alain Clement's PhD thesis ([Integral Cohomology of Finite Postnikov Towers](https://github.com/aclemen1/integral-cohomology-of-finite-postnikov-towers/blob/master/main.pdf), see Appendix B, pp. 87-89). The original C++ sources are preserved in the `legacy/` directory.

## Build

```
cabal build
```

## Test

```
cabal test
```

## Run

Interactive mode:
```
cabal run emm
```

CLI mode (outputs LaTeX to stdout):
```
cabal run emm -- [s] [n] [range]
```

Parameters: `s` = log-order of cyclic group Z/2^s, `n` = connectivity+1, `range` = upper bound for computation.

## Docker

```
docker build -t aclemen1/emm .
docker run -it --rm aclemen1/emm
docker run -it --rm aclemen1/emm emm 1 2 10
```
