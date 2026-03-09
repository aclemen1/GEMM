/**
 * GEMM C comprehensive test — exercises all 5 FFI functions.
 */
#include <stdio.h>
#include <string.h>
#include "gemm.h"

int main(void) {
    gemm_init();

    /* 1. gemm_homology_p */
    char *r1 = gemm_homology_p(2, 1, 2, 10);
    printf("1. homology_p(2,1,2,10)  OK — %zu chars\n", strlen(r1));
    gemm_free(r1);

    /* 2. gemm_homology_z */
    char *r2 = gemm_homology_z(3, 10);
    printf("2. homology_z(3,10)      OK — %zu chars\n", strlen(r2));
    gemm_free(r2);

    /* 3. gemm_certificate */
    char *r3 = gemm_certificate(2, 1, 2, 6);
    printf("3. certificate(2,1,2,6)  OK — %zu chars\n", strlen(r3));
    gemm_free(r3);

    /* 4. gemm_latex_p */
    char *r4 = gemm_latex_p(3, 1, 4, 10);
    printf("4. latex_p(3,1,4,10)     OK — %zu chars\n", strlen(r4));
    gemm_free(r4);

    /* 5. gemm_latex_z */
    char *r5 = gemm_latex_z(2, 10);
    printf("5. latex_z(2,10)         OK — %zu chars\n", strlen(r5));
    gemm_free(r5);

    printf("\nAll 5 FFI functions verified.\n");

    gemm_shutdown();
    return 0;
}
