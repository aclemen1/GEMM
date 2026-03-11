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

## AI agent integration

The GEMM CLI is designed to be auto-discoverable by AI agents. All output is structured JSON (with `--json`), human messages go to stderr, and the built-in `schema` command lets agents introspect capabilities at runtime — no documentation pre-loading required.

### Schema introspection

```
gemm schema                # tool overview: commands, flags, output formats
gemm schema homology-p     # K(Z/p^f, n): parameters, constraints, response JSON Schema
gemm schema homology-z     # K(Z, n): parameters, constraints, response JSON Schema
gemm schema certificate    # Lean 4 certificate: parameters, constraints
```

An agent workflow typically looks like:

1. **Discover** — call `gemm schema` to list available commands
2. **Introspect** — call `gemm schema homology-p` to get parameter types, constraints, and the response JSON Schema
3. **Execute** — call `gemm --json 3 1 4 20` and parse the structured output
4. **Handle errors** — errors are returned as JSON to stderr with `code`, `message`, and `reason` fields

### Structured output

Every computation mode supports `--json` for machine-readable output. Response schemas are documented via `gemm schema <command>` and follow JSON Schema conventions (`type`, `properties`, `items`, `required`, `additionalProperties`).

### Agent skill example

```markdown
# GEMM — Compute homology of Eilenberg-MacLane spaces

## When to use
When the user asks about integral (co)homology groups of K(G, n) spaces,
homology exponents, or needs Lean 4 proof certificates.

## Discovery
Run `gemm schema` to list commands. Run `gemm schema <command>` for details.

## Quick reference
- `gemm --json p f n range` — H_*(K(Z/p^f, n); Z) as JSON
- `gemm --json Z n range`   — H_*(K(Z, n); Z) as JSON
- `gemm --cert p f n range` — Lean 4 proof certificate
```

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
