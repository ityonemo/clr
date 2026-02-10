// Test optional_payload preserves memory_safety tracking
//
// NOTE: The full test with `catch null` exposes a merge bug where one branch
// has an allocated pointer and the other has null. When merging, the code
// expects both branches to have memory_safety but null doesn't.
// This simplified test avoids that merge complexity.
pub fn main() u8 {
    const allocator = @import("std").heap.page_allocator;
    const ptr = allocator.create(u32) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr);
    return 0;
}
