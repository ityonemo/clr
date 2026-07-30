const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const defined = allocator.create(u8) catch return 1;
    const uninitialized = allocator.create(u8) catch {
        allocator.destroy(defined);
        return 2;
    };
    defined.* = 1;

    var choose_defined = true;
    _ = &choose_defined;
    const selected = if (choose_defined) defined else uninitialized;
    _ = selected.*;

    allocator.destroy(defined);
    allocator.destroy(uninitialized);
    return 0;
}
