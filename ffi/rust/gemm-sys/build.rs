fn main() {
    // Set GEMM_LIB_DIR to the directory containing libgemm-ffi.dylib/.so
    let lib_dir = std::env::var("GEMM_LIB_DIR")
        .unwrap_or_else(|_| "/usr/local/lib".to_string());
    println!("cargo:rustc-link-search=native={}", lib_dir);
    println!("cargo:rustc-link-lib=dylib=gemm-ffi");
}
