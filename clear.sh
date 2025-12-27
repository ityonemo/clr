#!/bin/bash
# Remove generated .air.zig files and executables from project root
rm -f *.air.zig *.air *.o *.a
# Remove any test executables (files without extensions that are executables)
find . -maxdepth 1 -type f -executable ! -name "*.sh" -delete
echo "Cleaned up generated files"
