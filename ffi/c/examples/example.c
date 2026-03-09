/**
 * GEMM C example — compute homology of Eilenberg-MacLane spaces.
 *
 * Build:
 *   gcc -o example example.c -I../../include -L../../lib -lgemm-ffi
 *
 * Run (macOS):
 *   DYLD_LIBRARY_PATH=../../lib ./example
 */
#include <stdio.h>
#include "gemm.h"

int main(void) {
    gemm_init();

    /* Homology of K(Z/2, 2) */
    char *json = gemm_homology_p(2, 1, 2, 10);
    printf("H*(K(Z/2, 2); Z) =\n%s\n", json);
    gemm_free(json);

    /* Homology of K(Z, 3) */
    char *json2 = gemm_homology_z(3, 10);
    printf("H*(K(Z, 3); Z) =\n%s\n", json2);
    gemm_free(json2);

    /* Lean certificate for K(Z/2, 2) */
    char *cert = gemm_certificate(2, 1, 2, 10);
    printf("Certificate:\n%.200s...\n", cert);
    gemm_free(cert);

    gemm_shutdown();
    return 0;
}
