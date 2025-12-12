#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

ZIG_COMPILER="${SCRIPT_DIR}/zig/zig-out/bin/zig"
LIBCLR="${SCRIPT_DIR}/zig-out/lib/libclr.so"
LIB_DIR="${SCRIPT_DIR}/lib"

if [ -z "$1" ]; then
    echo "Usage: $0 <file.zig>"
    exit 1
fi

INPUT="$1"
BASENAME="$(basename "${INPUT%.zig}")"
OUTPUT="${SCRIPT_DIR}/${BASENAME}.air.zig"

"$ZIG_COMPILER" build-exe \
    -fair-out="$LIBCLR" \
    -ofmt=air \
    -femit-bin="$OUTPUT" \
    "$INPUT"

zig run --dep clr -Mroot="$OUTPUT" -Mclr="${LIB_DIR}/lib.zig"
