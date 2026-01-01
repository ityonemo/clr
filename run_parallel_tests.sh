#!/bin/bash

PROJECT_ROOT="$(cd "$(dirname "$0")" && pwd)"
ZIG_COMPILER="${PROJECT_ROOT}/zig/zig-out/bin/zig"
LIBCLR="${PROJECT_ROOT}/zig-out/lib/libclr.so"
LIB_DIR="${PROJECT_ROOT}/lib"
TEMP_DIR=$(mktemp -d)
MAX_JOBS=8
running=0
pids=()

cleanup() {
    # Kill all background jobs
    for pid in "${pids[@]}"; do
        kill "$pid" 2>/dev/null
    done
    rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

run_test() {
    local input="$1"
    local basename="$(basename "$input" .zig)"
    local test_dir="${TEMP_DIR}/${basename}_$$_${RANDOM}"
    mkdir -p "$test_dir"
    local air_file="${test_dir}/${basename}.air.zig"

    echo "=== $input ==="

    # Only generate AIR - don't run analysis
    "$ZIG_COMPILER" build-exe \
        -fair-out="$LIBCLR" \
        -ofmt=air \
        -femit-bin="$air_file" \
        --global-cache-dir "$test_dir/.zig-cache" \
        -lc \
        "$input" 2>&1
}

for test_file in $(find test/cases -name "*.zig"); do
    run_test "$test_file" &
    pids+=($!)
    running=$((running + 1))

    if [ $running -ge $MAX_JOBS ]; then
        # Wait for any job, check exit status
        wait -n
        status=$?
        running=$((running - 1))
        # Only stop on crash signals (134=SIGABRT, 139=SIGSEGV, 136=SIGFPE, 137=SIGKILL)
        if [ $status -ge 128 ]; then
            echo "CRASH DETECTED (exit $status, signal $((status - 128))) - stopping all tests"
            exit 1
        fi
    fi
done

# Wait for remaining jobs
while [ $running -gt 0 ]; do
    wait -n
    status=$?
    running=$((running - 1))
    if [ $status -ge 128 ]; then
        echo "CRASH DETECTED (exit $status, signal $((status - 128))) - stopping all tests"
        exit 1
    fi
done

echo "All tests complete"
