/*
 * C shim bridging the GEMM FFI to R's .Call interface.
 *
 * Each function:
 *   1. Extracts integer arguments from SEXP
 *   2. Calls the corresponding gemm_* C function
 *   3. Copies the returned string into an R STRSXP
 *   4. Frees the GEMM-allocated string via gemm_free
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Declarations from gemm.h */
extern void gemm_init(void);
extern void gemm_shutdown(void);
extern char *gemm_homology_p(int p, int f, int n, int range);
extern char *gemm_homology_z(int n, int range);
extern char *gemm_certificate(int p, int f, int n, int range);
extern char *gemm_latex_p(int p, int f, int n, int range);
extern char *gemm_latex_z(int n, int range);
extern void gemm_free(void *ptr);

SEXP R_gemm_init(void) {
    gemm_init();
    return R_NilValue;
}

SEXP R_gemm_shutdown(void) {
    gemm_shutdown();
    return R_NilValue;
}

static SEXP wrap_string(char *s) {
    if (!s) {
        Rf_error("GEMM returned NULL");
        return R_NilValue;
    }
    SEXP result = PROTECT(Rf_mkString(s));
    gemm_free(s);
    UNPROTECT(1);
    return result;
}

SEXP R_gemm_homology_p(SEXP p, SEXP f, SEXP n, SEXP range) {
    return wrap_string(gemm_homology_p(
        Rf_asInteger(p), Rf_asInteger(f),
        Rf_asInteger(n), Rf_asInteger(range)));
}

SEXP R_gemm_homology_z(SEXP n, SEXP range) {
    return wrap_string(gemm_homology_z(
        Rf_asInteger(n), Rf_asInteger(range)));
}

SEXP R_gemm_certificate(SEXP p, SEXP f, SEXP n, SEXP range) {
    return wrap_string(gemm_certificate(
        Rf_asInteger(p), Rf_asInteger(f),
        Rf_asInteger(n), Rf_asInteger(range)));
}

SEXP R_gemm_latex_p(SEXP p, SEXP f, SEXP n, SEXP range) {
    return wrap_string(gemm_latex_p(
        Rf_asInteger(p), Rf_asInteger(f),
        Rf_asInteger(n), Rf_asInteger(range)));
}

SEXP R_gemm_latex_z(SEXP n, SEXP range) {
    return wrap_string(gemm_latex_z(
        Rf_asInteger(n), Rf_asInteger(range)));
}

/* Registration table for .Call */
static const R_CallMethodDef callMethods[] = {
    {"R_gemm_init",        (DL_FUNC) &R_gemm_init,        0},
    {"R_gemm_shutdown",    (DL_FUNC) &R_gemm_shutdown,    0},
    {"R_gemm_homology_p",  (DL_FUNC) &R_gemm_homology_p,  4},
    {"R_gemm_homology_z",  (DL_FUNC) &R_gemm_homology_z,  2},
    {"R_gemm_certificate", (DL_FUNC) &R_gemm_certificate, 4},
    {"R_gemm_latex_p",     (DL_FUNC) &R_gemm_latex_p,     4},
    {"R_gemm_latex_z",     (DL_FUNC) &R_gemm_latex_z,     2},
    {NULL, NULL, 0}
};

void R_init_gemm_r(DllInfo *info) {
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
