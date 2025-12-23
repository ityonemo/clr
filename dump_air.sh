#!/bin/bash
# Dump AIR for a specific function from a Zig source file
# Usage: ./dump_air.sh <source_file> <function_name> [num_lines]
#
# Example: ./dump_air.sh test/cases/undefined/basic/assigned_before_use.zig assigned_before_use.main 40

if [ $# -lt 2 ]; then
    echo "Usage: $0 <source_file> <function_name> [num_lines]"
    echo "Example: $0 test/cases/undefined/basic/assigned_before_use.zig assigned_before_use.main 40"
    exit 1
fi

SOURCE_FILE="$1"
FUNC_NAME="$2"
NUM_LINES="${3:-50}"

zig/zig-out/bin/zig build-exe --verbose-air "$SOURCE_FILE" 2>&1 | grep -A "$NUM_LINES" "# Begin Function AIR: $FUNC_NAME"
