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

zig/zig-out/bin/zig build-exe -ofmt=air "$SOURCE_FILE" 2>&1 | grep -A "$NUM_LINES" "# Begin Function AIR: $FUNCTION_NAME"
