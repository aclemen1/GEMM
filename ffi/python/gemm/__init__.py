"""
GEMM — Generalized Eilenberg-MacLane Machine (Python bindings)

Computes integral homology/cohomology of Eilenberg-MacLane spaces K(G, n).
Uses the Haskell GEMM library via C FFI.

Usage:
    import gemm
    result = gemm.homology_p(2, 1, 2, 20)
    print(result["homology"])
"""

import atexit
import ctypes
import json
import os
import sys

# Locate the shared library
_lib_path = os.environ.get("GEMM_LIB_PATH")
if _lib_path is None:
    _dir = os.path.dirname(os.path.abspath(__file__))
    if sys.platform == "darwin":
        _lib_path = os.path.join(_dir, "libgemm-ffi.dylib")
    elif sys.platform == "win32":
        _lib_path = os.path.join(_dir, "gemm-ffi.dll")
    else:
        _lib_path = os.path.join(_dir, "libgemm-ffi.so")

_lib = ctypes.CDLL(_lib_path)

# Declare function signatures
# Return type is c_void_p (not c_char_p) to retain pointer for gemm_free.
_lib.gemm_homology_p.argtypes = [ctypes.c_int] * 4
_lib.gemm_homology_p.restype = ctypes.c_void_p

_lib.gemm_homology_z.argtypes = [ctypes.c_int] * 2
_lib.gemm_homology_z.restype = ctypes.c_void_p

_lib.gemm_certificate.argtypes = [ctypes.c_int] * 4
_lib.gemm_certificate.restype = ctypes.c_void_p

_lib.gemm_latex_p.argtypes = [ctypes.c_int] * 4
_lib.gemm_latex_p.restype = ctypes.c_void_p

_lib.gemm_latex_z.argtypes = [ctypes.c_int] * 2
_lib.gemm_latex_z.restype = ctypes.c_void_p

_lib.gemm_free.argtypes = [ctypes.c_void_p]
_lib.gemm_free.restype = None

_lib.gemm_init.argtypes = []
_lib.gemm_init.restype = None

_lib.gemm_shutdown.argtypes = []
_lib.gemm_shutdown.restype = None

# Initialise GEMM library
_lib.gemm_init()
atexit.register(_lib.gemm_shutdown)


def _call_and_free(ptr):
    """Read a C string from pointer, free it, return Python string."""
    if not ptr:
        raise RuntimeError("GEMM returned null pointer")
    try:
        result = ctypes.string_at(ptr).decode("utf-8")
    finally:
        _lib.gemm_free(ptr)
    return result


def _check_error(s):
    """Raise if the result is a JSON error."""
    if s.startswith('{"error":'):
        raise RuntimeError(json.loads(s)["error"])
    return s


def homology_p(p: int, f: int, n: int, range: int) -> dict:
    """Compute H*(K(Z/p^f, n); Z) and return parsed JSON."""
    ptr = _lib.gemm_homology_p(p, f, n, range)
    s = _check_error(_call_and_free(ptr))
    return json.loads(s)


def homology_z(n: int, range: int) -> dict:
    """Compute H*(K(Z, n); Z) and return parsed JSON."""
    ptr = _lib.gemm_homology_z(n, range)
    s = _check_error(_call_and_free(ptr))
    return json.loads(s)


def certificate(p: int, f: int, n: int, range: int) -> str:
    """Generate a Lean 4 proof certificate for K(Z/p^f, n)."""
    ptr = _lib.gemm_certificate(p, f, n, range)
    return _check_error(_call_and_free(ptr))


def latex_p(p: int, f: int, n: int, range: int) -> str:
    """Generate a LaTeX document for K(Z/p^f, n)."""
    ptr = _lib.gemm_latex_p(p, f, n, range)
    return _check_error(_call_and_free(ptr))


def latex_z(n: int, range: int) -> str:
    """Generate a LaTeX document for K(Z, n)."""
    ptr = _lib.gemm_latex_z(n, range)
    return _check_error(_call_and_free(ptr))
