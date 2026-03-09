//! Raw FFI bindings to the GEMM Haskell library.
//!
//! All returned `*mut c_char` pointers must be freed with `gemm_free`.

use std::os::raw::{c_char, c_int, c_void};

extern "C" {
    pub fn gemm_init();
    pub fn gemm_shutdown();

    pub fn gemm_homology_p(p: c_int, f: c_int, n: c_int, range: c_int) -> *mut c_char;
    pub fn gemm_homology_z(n: c_int, range: c_int) -> *mut c_char;
    pub fn gemm_certificate(p: c_int, f: c_int, n: c_int, range: c_int) -> *mut c_char;
    pub fn gemm_latex_p(p: c_int, f: c_int, n: c_int, range: c_int) -> *mut c_char;
    pub fn gemm_latex_z(n: c_int, range: c_int) -> *mut c_char;
    pub fn gemm_free(ptr: *mut c_void);
}
