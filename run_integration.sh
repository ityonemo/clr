#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Optimization level for libclr (must match vendored zig build)
# Override with: OPTIMIZE=ReleaseSafe ./run_integration.sh
# Use OPTIMIZE=Debug for debug builds (or leave empty)
OPTIMIZE="${OPTIMIZE:-ReleaseFast}"

# make sure we are running the most recent version
if [ "$OPTIMIZE" = "Debug" ] || [ -z "$OPTIMIZE" ]; then
    zig build
else
    zig build -Doptimize="$OPTIMIZE"
fi

# Check if bats is available
if command -v bats &> /dev/null; then
    # Try parallel execution if GNU parallel is available
    # GNU parallel is required (not moreutils parallel)
    if parallel --version 2>&1 | grep -q "GNU parallel"; then
        bats -j "$(nproc)" "$SCRIPT_DIR/test/integration/"
    else
        echo "Note: Install GNU parallel for faster test execution (sudo apt install parallel)"
        bats "$SCRIPT_DIR/test/integration/"
    fi
else
    echo "BATS not installed. Install with:"
    echo "  Ubuntu/Debian: sudo apt install bats"
    echo "  macOS: brew install bats-core"
    echo "  Or: git clone https://github.com/bats-core/bats-core.git && cd bats-core && sudo ./install.sh /usr/local"
    exit 1
fi
