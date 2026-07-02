const std = @import("std");

const Owner = struct {
    item: *u32,
};

fn first(owner: Owner) *u32 {
    return owner.item;
}

pub fn main() u8 {
    const item = std.heap.page_allocator.create(u32) catch return 1;
    defer std.heap.page_allocator.destroy(item);
    item.* = 2;

    const returned = first(.{ .item = item });
    return @intCast(returned.* - 2);
}
