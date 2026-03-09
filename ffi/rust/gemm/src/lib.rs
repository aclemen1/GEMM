//! Safe Rust bindings to the GEMM (Generalized Eilenberg-MacLane Machine).
//!
//! # Example
//! ```no_run
//! let result = gemm::homology_p(2, 1, 2, 20).unwrap();
//! println!("{}", result);
//! ```

use std::ffi::CStr;
use std::os::raw::c_void;
use std::sync::OnceLock;

/// Error type for GEMM operations.
#[derive(Debug)]
pub enum GemmError {
    NullPointer,
    Utf8Error(std::str::Utf8Error),
    Computation(String),
}

impl std::fmt::Display for GemmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GemmError::NullPointer => write!(f, "GEMM returned null pointer"),
            GemmError::Utf8Error(e) => write!(f, "UTF-8 error: {}", e),
            GemmError::Computation(msg) => write!(f, "GEMM error: {}", msg),
        }
    }
}

impl std::error::Error for GemmError {}

static INIT: OnceLock<()> = OnceLock::new();

fn ensure_init() {
    INIT.get_or_init(|| unsafe {
        gemm_sys::gemm_init();
    });
}

/// RAII guard for a GEMM-allocated C string.
struct GemmString(*mut i8);

impl Drop for GemmString {
    fn drop(&mut self) {
        if !self.0.is_null() {
            unsafe { gemm_sys::gemm_free(self.0 as *mut c_void) };
        }
    }
}

fn read_result(ptr: *mut i8) -> Result<String, GemmError> {
    if ptr.is_null() {
        return Err(GemmError::NullPointer);
    }
    let guard = GemmString(ptr);
    let cstr = unsafe { CStr::from_ptr(guard.0) };
    let s = cstr.to_str().map_err(GemmError::Utf8Error)?.to_owned();
    if s.starts_with("{\"error\":") {
        let v: serde_json::Value = serde_json::from_str(&s).unwrap_or_default();
        let msg = v["error"].as_str().unwrap_or("unknown error").to_owned();
        return Err(GemmError::Computation(msg));
    }
    Ok(s)
}

/// Compute H*(K(Z/p^f, n); Z) and return JSON string.
pub fn homology_p(p: i32, f: i32, n: i32, range: i32) -> Result<serde_json::Value, GemmError> {
    ensure_init();
    let ptr = unsafe { gemm_sys::gemm_homology_p(p, f, n, range) };
    let s = read_result(ptr)?;
    serde_json::from_str(&s).map_err(|e| GemmError::Computation(e.to_string()))
}

/// Compute H*(K(Z, n); Z) and return JSON string.
pub fn homology_z(n: i32, range: i32) -> Result<serde_json::Value, GemmError> {
    ensure_init();
    let ptr = unsafe { gemm_sys::gemm_homology_z(n, range) };
    let s = read_result(ptr)?;
    serde_json::from_str(&s).map_err(|e| GemmError::Computation(e.to_string()))
}

/// Generate a Lean 4 proof certificate for K(Z/p^f, n).
pub fn certificate(p: i32, f: i32, n: i32, range: i32) -> Result<String, GemmError> {
    ensure_init();
    let ptr = unsafe { gemm_sys::gemm_certificate(p, f, n, range) };
    read_result(ptr)
}

/// Generate a LaTeX document for K(Z/p^f, n).
pub fn latex_p(p: i32, f: i32, n: i32, range: i32) -> Result<String, GemmError> {
    ensure_init();
    let ptr = unsafe { gemm_sys::gemm_latex_p(p, f, n, range) };
    read_result(ptr)
}

/// Generate a LaTeX document for K(Z, n).
pub fn latex_z(n: i32, range: i32) -> Result<String, GemmError> {
    ensure_init();
    let ptr = unsafe { gemm_sys::gemm_latex_z(n, range) };
    read_result(ptr)
}
