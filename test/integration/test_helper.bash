#!/usr/bin/env bash

# BATS test helper for CLR integration tests

# Project root directory
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

# Paths
ZIG_COMPILER="${PROJECT_ROOT}/zig/zig-out/bin/zig"
LIBCLR="${PROJECT_ROOT}/zig-out/lib/libclr.so"
TEST_CASES="${PROJECT_ROOT}/test/cases"
LIB_DIR="${PROJECT_ROOT}/lib"

# Temporary directory for test artifacts
setup() {
    TEST_TEMP="$(mktemp -d)"
}

teardown() {
    # On failure, copy .air.zig files to project root for inspection
    if [ "$BATS_TEST_COMPLETED" != 1 ]; then
        cp "$TEST_TEMP"/*.air.zig "$PROJECT_ROOT/" 2>/dev/null || true
    fi
    rm -rf "$TEST_TEMP"
}

# Compile a test case with AIR output
# Usage: compile_air <input.zig> <output.air>
compile_air() {
    local input="$1"
    local output="$2"
    "$ZIG_COMPILER" build-exe \
        -fair-out="$LIBCLR" \
        -ofmt=air \
        -femit-bin="$output" \
        --global-cache-dir "$TEST_TEMP/.zig-cache" \
        -lc \
        "$input" 2>&1
}

# Run generated AIR as Zig code
# Usage: run_air <file.air.zig>
run_air() {
    local zig_file="$1"
    zig run \
        --dep clr \
        -Mroot="$zig_file" \
        -Mclr="${LIB_DIR}/lib.zig" \
        --global-cache-dir "$TEST_TEMP/.zig-cache" \
        2>&1
}

# Full compile-and-run pipeline
# Usage: compile_and_run <input.zig>
compile_and_run() {
    local input="$1"
    local basename="$(basename "$input" .zig)"
    local air_file="${TEST_TEMP}/${basename}.air.zig"

    compile_air "$input" "$air_file" >/dev/null
    run_air "$air_file"
}
