#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# make sure we are running the most recent version
zig build

# Check if bats is available
if command -v bats &> /dev/null; then
    bats "$SCRIPT_DIR/test/integration/"
else
    echo "BATS not installed. Install with:"
    echo "  Ubuntu/Debian: sudo apt install bats"
    echo "  macOS: brew install bats-core"
    echo "  Or: git clone https://github.com/bats-core/bats-core.git && cd bats-core && sudo ./install.sh /usr/local"
    exit 1
fi
