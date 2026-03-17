const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var selector: u8 = 0;
    _ = &selector;

    var ptr: *u8 = undefined;

    switch (selector) {
        0 => {
            ptr = allocator.create(u8) catch return 1;
            ptr = allocator.create(u8) catch return 1; // clobber - first allocation leaked
        },
        1 => {
            ptr = allocator.create(u8) catch return 1;
        },
        else => {
            ptr = allocator.create(u8) catch return 1;
        },
    }

    allocator.destroy(ptr);
    return 0;
}
