const std = @import("std");

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr);

    // Dereference freed pointer in format tuple - this triggers use-after-free
    var buf: [100]u8 = undefined;
    _ = std.fmt.bufPrint(&buf, "{}", .{ptr.*}) catch unreachable;

    return 0;
}
