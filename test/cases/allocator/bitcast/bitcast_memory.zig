// Test bitcast preserves memory_safety tracking
pub fn main() u8 {
    const allocator = @import("std").heap.page_allocator;
    const ptr = allocator.create(u32) catch return 1;

    const byte_ptr: *u8 = @ptrCast(ptr); // bitcast
    byte_ptr.* = 42;

    allocator.destroy(ptr);
    return 0;
}
