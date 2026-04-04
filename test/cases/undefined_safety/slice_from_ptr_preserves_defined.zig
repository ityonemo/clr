// Test that creating a slice from a pointer preserves the defined state.
// This is the pattern used by std.mem.sliceAsBytes.

const std = @import("std");

fn sliceAsBytes(slice: []const u8) []const u8 {
    // This is a simplified version of std.mem.sliceAsBytes
    // It extracts the pointer, then creates a new slice from it.
    const ptr = slice.ptr;
    const len = slice.len;
    return ptr[0..len];
}

fn eqlBytes(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    if (a.len == 0) return true;
    // Access elements - this should NOT report undefined
    return a[0] == b[0];
}

pub fn main() u8 {
    const data1 = "hello";
    const data2 = "hello";

    const bytes1 = sliceAsBytes(data1);
    const bytes2 = sliceAsBytes(data2);

    if (eqlBytes(bytes1, bytes2)) {
        return 0;
    }
    return 1;
}
