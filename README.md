# The Generalized Eilenberg-MacLane Machine (GEMM)

The Generalized Eilenberg-MacLane Machine (GEMM) computes the integral homology and cohomology groups of Eilenberg-MacLane spaces K(Z/p^f, n) for any prime p, and K(Z, n).

This is a Haskell implementation based on the theoretical results of:

- H. Cartan, *Algèbres d'Eilenberg-MacLane et homotopie*, Exposés 2 à 16, Séminaire Henri Cartan, École Normale Supérieure, Paris, 1956.
- N. Pointet-Tischler, *Invariants de Postnikov des espaces de lacets*, Ph.D. thesis, University of Lausanne, Switzerland, 1996.
- A. Clément, *Integral Cohomology of Finite Postnikov Towers*, Ph.D. thesis, University of Lausanne, Switzerland, 2002. ([PDF](https://github.com/aclemen1/integral-cohomology-of-finite-postnikov-towers/blob/master/main.pdf))

The original C++ sources (version 2.1, 2002) are preserved in the `legacy/` directory.

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
cabal run gemm
```

CLI mode (outputs LaTeX to stdout):
```
cabal run gemm -- p f n range      # K(Z/p^f, n)
cabal run gemm -- s n range        # K(Z/2^s, n), backward compatible
cabal run gemm -- Z n range        # K(Z, n)
```

JSON output:
```
cabal run gemm -- --json p f n range
cabal run gemm -- --json Z n range
```

Parameters: `p` = prime, `f` = log-order of cyclic group Z/p^f, `n` = connectivity+1, `range` = upper bound for computation.

## Docker

```
docker build -t aclemen1/gemm .
docker run -it --rm aclemen1/gemm
docker run -it --rm aclemen1/gemm gemm 2 1 2 10
```
