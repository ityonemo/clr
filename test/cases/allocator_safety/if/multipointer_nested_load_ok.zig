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

    var choose_stack = true;
    _ = &choose_stack;
    if (choose_stack) {
        outer.* = &stack_value;
    } else {
        outer.* = heap_value;
    }

    const selected = outer.*;
    _ = selected.*;

    allocator.destroy(outer);
    allocator.destroy(heap_value);
    return 0;
}
