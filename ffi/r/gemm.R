# GEMM — Generalized Eilenberg-MacLane Machine (R bindings)
#
# Uses the GEMM shared library via a C shim and R's .Call interface.
#
# Usage:
#   source("gemm.R")
#   result <- gemm_homology_p(2, 1, 2, 20)

.gemm_env <- new.env(parent = emptyenv())
.gemm_env$initialized <- FALSE

gemm_load <- function(shim_path = NULL, lib_path = NULL) {
  if (.gemm_env$initialized) return(invisible(NULL))

  # Find the R shim .so
  if (is.null(shim_path)) {
    shim_path <- Sys.getenv("GEMM_SHIM_PATH", "")
    if (shim_path == "") {
      # Default: look next to this script
      pkg_dir <- Sys.getenv("GEMM_R_DIR", "")
      if (pkg_dir == "") {
        pkg_dir <- dirname(sys.frame(sys.nframe())$ofile)
      }
      shim_path <- file.path(pkg_dir, "gemm_r.so")
    }
  }

  # Ensure libgemm-ffi is loadable (must be on DYLD_LIBRARY_PATH or loaded first)
  if (!is.null(lib_path) || Sys.getenv("GEMM_LIB_PATH", "") != "") {
    lp <- if (!is.null(lib_path)) lib_path else Sys.getenv("GEMM_LIB_PATH")
    dyn.load(lp)
  }

  dyn.load(shim_path)
  .Call("R_gemm_init")
  .gemm_env$initialized <- TRUE
  invisible(NULL)
}

.ensure_loaded <- function() {
  if (!.gemm_env$initialized) gemm_load()
}

#' Compute H*(K(Z/p^f, n); Z) and return parsed JSON.
gemm_homology_p <- function(p, f, n, range) {
  .ensure_loaded()
  json_str <- .Call("R_gemm_homology_p",
    as.integer(p), as.integer(f), as.integer(n), as.integer(range))
  jsonlite::fromJSON(json_str)
}

#' Compute H*(K(Z, n); Z) and return parsed JSON.
gemm_homology_z <- function(n, range) {
  .ensure_loaded()
  json_str <- .Call("R_gemm_homology_z", as.integer(n), as.integer(range))
  jsonlite::fromJSON(json_str)
}

#' Generate a Lean 4 proof certificate for K(Z/p^f, n).
gemm_certificate <- function(p, f, n, range) {
  .ensure_loaded()
  .Call("R_gemm_certificate",
    as.integer(p), as.integer(f), as.integer(n), as.integer(range))
}

#' Generate a LaTeX document for K(Z/p^f, n).
gemm_latex_p <- function(p, f, n, range) {
  .ensure_loaded()
  .Call("R_gemm_latex_p",
    as.integer(p), as.integer(f), as.integer(n), as.integer(range))
}

#' Generate a LaTeX document for K(Z, n).
gemm_latex_z <- function(n, range) {
  .ensure_loaded()
  .Call("R_gemm_latex_z", as.integer(n), as.integer(range))
}
