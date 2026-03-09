#include <stddef.h>
#include "HsFFI.h"

static int gemm_initialized = 0;

void gemm_init(void) {
    if (!gemm_initialized) {
        int argc = 1;
        char *argv[] = { "gemm", NULL };
        char **pargv = argv;
        hs_init(&argc, &pargv);
        gemm_initialized = 1;
    }
}

void gemm_shutdown(void) {
    if (gemm_initialized) {
        hs_exit();
        gemm_initialized = 0;
    }
}
