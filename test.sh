#!/bin/sh

zig/zig-out/bin/zig build-exe -fair-out=zig-out/lib/libclr.so -ofmt=air foo.zig
