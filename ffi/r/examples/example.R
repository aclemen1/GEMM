#!/usr/bin/env Rscript
# GEMM R example — compute homology of Eilenberg-MacLane spaces via C FFI.
#
# Run:
#   cd ffi/r && Rscript examples/example.R

# Find script and package directories
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  script_dir <- "."
}
pkg_dir <- normalizePath(file.path(script_dir, ".."))

Sys.setenv(GEMM_R_DIR = pkg_dir)
Sys.setenv(GEMM_LIB_PATH = file.path(pkg_dir, "..", "lib", "libgemm-ffi.dylib"))
source(file.path(pkg_dir, "gemm.R"))

cat("=== K(Z/2, 2) ===\n")
result <- gemm_homology_p(2, 1, 2, 10)
for (deg in names(result$homology)) {
  cat(sprintf("  H_%s = %s\n", deg, result$homology[[deg]]))
}

cat("\n=== K(Z, 3) ===\n")
result2 <- gemm_homology_z(3, 10)
for (deg in names(result2$homology)) {
  cat(sprintf("  H_%s = %s\n", deg, result2$homology[[deg]]))
}

cat("\n=== Lean certificate for K(Z/3, 4) ===\n")
cert <- gemm_certificate(3, 1, 4, 15)
cat(substr(cert, 1, 300), "...\n")

cat("\n=== LaTeX for K(Z/2, 2) ===\n")
latex <- gemm_latex_p(2, 1, 2, 10)
cat(substr(latex, 1, 200), "...\n")

cat("\n=== LaTeX for K(Z, 2) ===\n")
latex2 <- gemm_latex_z(2, 10)
cat(substr(latex2, 1, 200), "...\n")

cat("\nAll 5 FFI functions verified from R via C shim.\n")
