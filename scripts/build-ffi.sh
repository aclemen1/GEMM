#!/bin/bash
# Build the GEMM FFI shared library and copy it to ffi/lib/.
set -euo pipefail

cd "$(dirname "$0")/.."

echo "Building gemm-ffi shared library..."
cabal build flib:gemm-ffi

# Find the built library
LIB=$(find dist-newstyle -name 'libgemm-ffi.dylib' -o -name 'libgemm-ffi.so' 2>/dev/null | head -1)
if [ -z "$LIB" ]; then
    echo "Error: shared library not found."
    exit 1
fi

mkdir -p ffi/lib
cp "$LIB" ffi/lib/
echo "Copied to ffi/lib/$(basename "$LIB")"

# Show how to use
echo ""
echo "Usage:"
echo "  export GEMM_LIB_PATH=$(pwd)/ffi/lib/$(basename "$LIB")"
echo ""
echo "  # Python"
echo "  cd ffi/python && python examples/example.py"
echo ""
echo "  # C"
echo "  cd ffi/c/examples"
echo "  gcc -o example example.c -I../../include -L../../lib -lgemm-ffi"
echo "  DYLD_LIBRARY_PATH=../../lib ./example"
