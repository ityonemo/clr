// Test basic std.HashMap usage without false positives
const std = @import("std");

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var map = std.AutoHashMap(u32, u32).init(allocator);
    defer map.deinit();

    // Put some items
    map.put(1, 100) catch return 1;
    map.put(2, 200) catch return 1;
    map.put(3, 300) catch return 1;

    // Get items
    if (map.get(1) != 100) return 2;
    if (map.get(2) != 200) return 3;
    if (map.get(3) != 300) return 4;

    // Check non-existent key
    if (map.get(999) != null) return 5;

    return 0;
}
