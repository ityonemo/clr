#!/bin/bash
# Generate .air.zig files for all integration test cases into /tmp/air

set -e

OUT_DIR="/tmp/air"
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

# Build libclr first
zig build

# Find all test case .zig files
for f in $(find test/cases -name "*.zig"); do
    if [ -f "$f" ]; then
        basename=$(basename "$f" .zig)
        subdir=$(dirname "$f" | sed 's|test/cases/||')

        # Create subdirectory structure
        mkdir -p "$OUT_DIR/$subdir"

        # Generate the .air.zig file
        echo "Processing: $f"
        zig/zig-out/bin/zig build-exe -fair-out=zig-out/lib/libclr.so -ofmt=air "$f" 2>/dev/null || true

        # Move the generated file to output dir
        if [ -f "${basename}.air.zig" ]; then
            mv "${basename}.air.zig" "$OUT_DIR/$subdir/"
        fi
    fi
done

echo ""
echo "Generated files in $OUT_DIR:"
find "$OUT_DIR" -name "*.air.zig" | sort
