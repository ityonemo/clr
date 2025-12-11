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
        "$input" 2>&1
}

# Run generated AIR as Zig code
# Usage: run_air <file.air>
run_air() {
    local air_file="$1"
    local zig_file="${air_file%.air}.zig"
    cp "$air_file" "$zig_file"
    zig run \
        --dep context \
        -Mroot="$zig_file" \
        -Mcontext="${LIB_DIR}/context.zig" \
        2>&1
}

# Full compile-and-run pipeline
# Usage: compile_and_run <input.zig>
compile_and_run() {
    local input="$1"
    local basename="$(basename "$input" .zig)"
    local air_file="${TEST_TEMP}/${basename}.air"

    compile_air "$input" "$air_file" >/dev/null
    run_air "$air_file"
}
