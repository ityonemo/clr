#!/bin/bash
# Dump AIR for a specific function
# Usage: ./dump_air.sh <source_file> <function_name> [num_lines]
# Requires debug build of zig compiler (--verbose-air only works in debug mode)
# Build with: cd zig && zig build --zig-lib-dir lib --prefix zig-out-debug

if [ $# -lt 2 ]; then
    echo "Usage: $0 <source_file> <function_name> [num_lines]"
    exit 1
fi

SOURCE_FILE="$1"
FUNCTION_NAME="$2"
NUM_LINES="${3:-40}"

# Use debug compiler (--verbose-air requires debug build)
ZIG_COMPILER="${ZIG_COMPILER:-zig/zig-out-debug/bin/zig}"

# Get basename without extension and prepend to function name
BASENAME=$(basename "$SOURCE_FILE" .zig)
FULL_FUNCTION_NAME="${BASENAME}.${FUNCTION_NAME}"

"$ZIG_COMPILER" build-exe --verbose-air -fno-error-tracing "$SOURCE_FILE" 2>&1 | grep -A "$NUM_LINES" "# Begin Function AIR: $FULL_FUNCTION_NAME"
