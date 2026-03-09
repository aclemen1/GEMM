# The Generalized Eilenberg-MacLane Machine (GEMM)

The Generalized Eilenberg-MacLane Machine (GEMM) computes the integral homology and cohomology groups of Eilenberg-MacLane spaces K(Z/p^f, n) for any prime p, and K(Z, n).

This is a Haskell implementation based on the theoretical results of:

- H. Cartan, *Algèbres d'Eilenberg-MacLane et homotopie*, Exposés 2 à 16, Séminaire Henri Cartan, École Normale Supérieure, Paris, 1956.
- N. Pointet-Tischler, *Invariants de Postnikov des espaces de lacets*, Ph.D. thesis, University of Lausanne, Switzerland, 1996.
- A. Clément, *Integral Cohomology of Finite Postnikov Towers*, Ph.D. thesis, University of Lausanne, Switzerland, 2002. ([PDF](https://github.com/aclemen1/integral-cohomology-of-finite-postnikov-towers/blob/master/main.pdf))

## Build

```
cabal build
```

## Test

```
cabal test
```

## Usage

### TUI (interactive mode)

```
cabal run gemm
```

The TUI provides menu navigation, parameter input, scrollable results with homology/cohomology toggle, and preview/export for LaTeX, JSON, and Lean certificates. Supports vim-style navigation (j/k/g/G).

### CLI

LaTeX output (default):
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

Lean 4 proof certificate:
```
cabal run gemm -- --cert p f n range
cabal run gemm -- --cert --name myDef p f n range
```

Parameters: `p` = prime, `f` = log-order of cyclic group Z/p^f, `n` = connectivity+1, `range` = upper bound for computation.

### Web interface

```
cabal run gemm-web
```

Opens a web server on port 3000 (configurable via `PORT` env var). Provides a browser-based interface with KaTeX-rendered results and LaTeX/JSON/certificate downloads.

### Language bindings (FFI)

The GEMM library is available as a C shared library (`libgemm-ffi`) callable from any language:

```
cabal build gemm-ffi
```

The shared library exports:

| Function | Returns |
|----------|---------|
| `gemm_homology_p(p, f, n, range)` | JSON string |
| `gemm_homology_z(n, range)` | JSON string |
| `gemm_certificate(p, f, n, range)` | Lean 4 source |
| `gemm_latex_p(p, f, n, range)` | LaTeX document |
| `gemm_latex_z(n, range)` | LaTeX document |
| `gemm_free(ptr)` | — |

All returned strings must be freed with `gemm_free`. Call `gemm_init()` before first use and `gemm_shutdown()` when done.

Wrappers and examples are provided for:

- **C** — `ffi/c/examples/example.c`
- **Python** — `ffi/python/gemm/` (ctypes, no compilation needed)
- **Rust** — `ffi/rust/gemm/` (safe wrapper with serde_json)
- **Julia** — `ffi/julia/GEMM/` (ccall + JSON3)
- **R** — `ffi/r/` (C shim via .Call + jsonlite)

Example (Python):
```python
import gemm
result = gemm.homology_p(2, 1, 2, 20)
print(result["homology"]["4"])  # \Z/2^{2}
```

Example (C):
```c
#include "gemm.h"
gemm_init();
char *json = gemm_homology_p(2, 1, 2, 20);
printf("%s\n", json);
gemm_free(json);
gemm_shutdown();
```

## Docker

```
docker build -t aclemen1/gemm .
docker run -it --rm aclemen1/gemm
docker run -it --rm aclemen1/gemm gemm 2 1 2 10
```
