// Test basic std.ArrayList usage without false positives
const std = @import("std");

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var list = std.ArrayList(u8).initCapacity(allocator, 0) catch return 1;
    defer list.deinit(allocator);

    // Append some items
    list.append(allocator, 1) catch return 1;
    list.append(allocator, 2) catch return 1;
    list.append(allocator, 3) catch return 1;

    // Access items
    if (list.items.len != 3) return 2;
    if (list.items[0] != 1) return 3;
    if (list.items[1] != 2) return 4;
    if (list.items[2] != 3) return 5;

    // Pop an item
    _ = list.pop();
    if (list.items.len != 2) return 6;

    // Clear
    list.clearRetainingCapacity();
    if (list.items.len != 0) return 7;

    return 0;
}
