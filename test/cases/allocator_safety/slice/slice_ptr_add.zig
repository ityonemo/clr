// Test case: Pointer arithmetic on slice ptr
// This should be valid - slice.ptr is a multi-item pointer

pub fn main() u8 {
    const data = [_]u8{ 1, 2, 3, 4, 5 };
    const slice: []const u8 = &data;

    // Get ptr from slice - this should be [*]const u8 (multi-item pointer)
    const ptr = slice.ptr;

    // Pointer arithmetic on multi-item pointer should be valid
    const second = ptr + 1;
    _ = second;

    return 0;
}
