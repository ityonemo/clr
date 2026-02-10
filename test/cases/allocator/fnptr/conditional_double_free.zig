const std = @import("std");

fn does_free(allocator: std.mem.Allocator, ptr: *u8) void {
    allocator.destroy(ptr);  // Double free in this branch
}

fn no_free(allocator: std.mem.Allocator, ptr: *u8) void {
    _ = allocator;
    _ = ptr;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    allocator.destroy(ptr);

    const cond = true;
    const callback: *const fn (std.mem.Allocator, *u8) void = if (cond) &does_free else &no_free;
    callback(allocator, ptr);  // One possible branch double-frees

    return 0;
}
