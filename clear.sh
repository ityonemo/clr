#!/bin/bash
# Clean up generated AIR files and executables from the project root
# Usage: ./clear.sh

# Remove .air.zig and .air files
rm -f *.air.zig *.air

# Remove executable files in project root (not directories, not scripts)
for f in *; do
    if [ -f "$f" ] && [ -x "$f" ] && [[ ! "$f" == *.sh ]] && [[ ! "$f" == *.bash ]]; then
        rm -f "$f"
    fi
done

echo "Cleaned up AIR and executable files"
