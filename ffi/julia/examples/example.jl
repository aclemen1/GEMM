# GEMM Julia example — compute homology of Eilenberg-MacLane spaces.
#
# Run:
#   GEMM_LIB_PATH=/path/to/libgemm-ffi.dylib julia --project=../GEMM example.jl

using GEMM

# H*(K(Z/2, 2); Z) up to degree 10
println("=== K(Z/2, 2) ===")
result = GEMM.homology_p(2, 1, 2, 10)
println(result)
println()

# H*(K(Z, 3); Z) up to degree 10
println("=== K(Z, 3) ===")
result = GEMM.homology_z(3, 10)
println(result)
println()

# Lean certificate for K(Z/3, 4)
println("=== Lean certificate for K(Z/3, 4) ===")
cert = GEMM.certificate(3, 1, 4, 15)
println(first(cert, 300), "...")
