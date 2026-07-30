const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const outer = allocator.create(*u8) catch return 1;
    var stack_value: u8 = 11;
    const heap_value = allocator.create(u8) catch {
        allocator.destroy(outer);
        return 2;
    };
    heap_value.* = 22;

    var choose_first = true;
    _ = &choose_first;
    if (choose_first) {
        outer.* = &stack_value;
    } else {
        outer.* = heap_value;
    }

    // `outer` is a **u8. Its owned allocation is unchanged even though the
    // inner *u8 has multiple possible sources after the branch merge.
    allocator.destroy(outer);
    allocator.destroy(heap_value);
    return 0;
}
