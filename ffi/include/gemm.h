#ifndef GEMM_H
#define GEMM_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialise the GEMM library. Must be called once before any gemm_* call.
 * Safe to call multiple times.
 */
void gemm_init(void);

/**
 * Shut down the GEMM library. Call once when done.
 */
void gemm_shutdown(void);

/**
 * Compute integral homology/cohomology of K(Z/p^f, n) up to given range.
 * Returns a JSON string (caller must free with gemm_free).
 */
char *gemm_homology_p(int p, int f, int n, int range);

/**
 * Compute integral homology/cohomology of K(Z, n) up to given range.
 * Returns a JSON string (caller must free with gemm_free).
 */
char *gemm_homology_z(int n, int range);

/**
 * Generate a Lean 4 proof certificate for K(Z/p^f, n).
 * Returns a Lean source string (caller must free with gemm_free).
 */
char *gemm_certificate(int p, int f, int n, int range);

/**
 * Generate a LaTeX document for K(Z/p^f, n).
 * Returns a LaTeX string (caller must free with gemm_free).
 */
char *gemm_latex_p(int p, int f, int n, int range);

/**
 * Generate a LaTeX document for K(Z, n).
 * Returns a LaTeX string (caller must free with gemm_free).
 */
char *gemm_latex_z(int n, int range);

/**
 * Free a string returned by any gemm_* function.
 * Safe to call with NULL.
 */
void gemm_free(void *ptr);

#ifdef __cplusplus
}
#endif

#endif /* GEMM_H */
