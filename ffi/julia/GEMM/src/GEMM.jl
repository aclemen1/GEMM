"""
    GEMM — Generalized Eilenberg-MacLane Machine (Julia bindings)

Computes integral homology/cohomology of Eilenberg-MacLane spaces K(G, n).
Uses the Haskell GEMM library via C FFI.

# Example
```julia
using GEMM
result = GEMM.homology_p(2, 1, 2, 20)
```
"""
module GEMM

using JSON3

const libgemm = Ref{String}()

function __init__()
    path = get(ENV, "GEMM_LIB_PATH", "")
    if isempty(path)
        if Sys.isapple()
            path = joinpath(@__DIR__, "..", "..", "libgemm-ffi.dylib")
        elseif Sys.iswindows()
            path = joinpath(@__DIR__, "..", "..", "gemm-ffi.dll")
        else
            path = joinpath(@__DIR__, "..", "..", "libgemm-ffi.so")
        end
    end
    libgemm[] = path
    ccall((:gemm_init, libgemm[]), Cvoid, ())
    atexit() do
        ccall((:gemm_shutdown, libgemm[]), Cvoid, ())
    end
end

function _call_and_free(ptr::Ptr{UInt8})
    ptr == C_NULL && error("GEMM returned null pointer")
    s = unsafe_string(ptr)
    ccall((:gemm_free, libgemm[]), Cvoid, (Ptr{UInt8},), ptr)
    startswith(s, "{\"error\":") && error(JSON3.read(s).error)
    return s
end

"""
    homology_p(p, f, n, range) -> Dict

Compute H*(K(Z/p^f, n); Z) and return parsed JSON.
"""
function homology_p(p::Int, f::Int, n::Int, range::Int)
    ptr = ccall((:gemm_homology_p, libgemm[]), Ptr{UInt8},
                (Cint, Cint, Cint, Cint), p, f, n, range)
    return JSON3.read(_call_and_free(ptr))
end

"""
    homology_z(n, range) -> Dict

Compute H*(K(Z, n); Z) and return parsed JSON.
"""
function homology_z(n::Int, range::Int)
    ptr = ccall((:gemm_homology_z, libgemm[]), Ptr{UInt8},
                (Cint, Cint), n, range)
    return JSON3.read(_call_and_free(ptr))
end

"""
    certificate(p, f, n, range) -> String

Generate a Lean 4 proof certificate for K(Z/p^f, n).
"""
function certificate(p::Int, f::Int, n::Int, range::Int)
    ptr = ccall((:gemm_certificate, libgemm[]), Ptr{UInt8},
                (Cint, Cint, Cint, Cint), p, f, n, range)
    return _call_and_free(ptr)
end

"""
    latex_p(p, f, n, range) -> String

Generate a LaTeX document for K(Z/p^f, n).
"""
function latex_p(p::Int, f::Int, n::Int, range::Int)
    ptr = ccall((:gemm_latex_p, libgemm[]), Ptr{UInt8},
                (Cint, Cint, Cint, Cint), p, f, n, range)
    return _call_and_free(ptr)
end

"""
    latex_z(n, range) -> String

Generate a LaTeX document for K(Z, n).
"""
function latex_z(n::Int, range::Int)
    ptr = ccall((:gemm_latex_z, libgemm[]), Ptr{UInt8},
                (Cint, Cint), n, range)
    return _call_and_free(ptr)
end

end # module
