const std = @import("std");

fn doSelect(stack_value: *u8, heap_value: *u8, choose_stack: bool) *u8 {
    return if (choose_stack) stack_value else heap_value;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var stack_value: u8 = 7;
    const heap_value = allocator.create(u8) catch return 1;
    heap_value.* = 9;

    var choose_stack = true;
    _ = &choose_stack;
    const value_ptr = doSelect(&stack_value, heap_value, choose_stack);
    allocator.destroy(value_ptr);
    return 0;
}
