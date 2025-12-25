#!/bin/bash
# Dump AIR for a specific function
# Usage: ./dump_air.sh <source_file> <function_name> [num_lines]

if [ $# -lt 2 ]; then
    echo "Usage: $0 <source_file> <function_name> [num_lines]"
    exit 1
fi

SOURCE_FILE="$1"
FUNCTION_NAME="$2"
NUM_LINES="${3:-40}"

# Get basename without extension and prepend to function name
BASENAME=$(basename "$SOURCE_FILE" .zig)
FULL_FUNCTION_NAME="${BASENAME}.${FUNCTION_NAME}"

zig/zig-out/bin/zig build-exe --verbose-air -fno-error-tracing "$SOURCE_FILE" 2>&1 | grep -A "$NUM_LINES" "# Begin Function AIR: $FULL_FUNCTION_NAME"
