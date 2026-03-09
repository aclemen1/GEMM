#!/usr/bin/env python3
"""
GEMM Python example — compute homology of Eilenberg-MacLane spaces.

Run from the ffi/python directory:
    GEMM_LIB_PATH=/path/to/libgemm-ffi.dylib python examples/example.py
"""
import os, sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
import gemm

# H*(K(Z/2, 2); Z) up to degree 10
print("=== K(Z/2, 2) ===")
result = gemm.homology_p(2, 1, 2, 10)
for deg, group in sorted(result["homology"].items(), key=lambda x: int(x[0])):
    print(f"  H_{deg} = {group}")

print()

# H*(K(Z, 3); Z) up to degree 10
print("=== K(Z, 3) ===")
result = gemm.homology_z(3, 10)
for deg, group in sorted(result["homology"].items(), key=lambda x: int(x[0])):
    print(f"  H_{deg} = {group}")

print()

# Lean certificate for K(Z/3, 4)
print("=== Lean certificate for K(Z/3, 4) ===")
cert = gemm.certificate(3, 1, 4, 15)
print(cert[:300] + "...")

print()

# LaTeX for K(Z/2, 2)
print("=== LaTeX for K(Z/2, 2) ===")
latex = gemm.latex_p(2, 1, 2, 10)
print(latex[:200] + "...")
