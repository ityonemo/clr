#!/bin/bash
# Run a pregenerated .air.zig file directly
# Usage: ./run_one_air.sh <air_file.air.zig>

set -e

if [ $# -lt 1 ]; then
    echo "Usage: $0 <air_file.air.zig>"
    exit 1
fi

AIR_FILE="$1"
LIB_DIR="lib"

if [ ! -f "$AIR_FILE" ]; then
    echo "Error: File not found: $AIR_FILE"
    exit 1
fi

zig run --dep clr -Mroot="$AIR_FILE" -Mclr="${LIB_DIR}/lib.zig"
