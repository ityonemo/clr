const std = @import("std");
const compiler = @import("compiler");
const InternPoolIndexSet = @import("InternPoolIndexSet.zig");

test "InternPoolIndexSet supports growth, membership, and removal" {
    var set = InternPoolIndexSet{};
    defer set.deinit(std.testing.allocator);

    for (100..140) |raw| {
        const index: compiler.InternPool.Index = @enumFromInt(raw);
        try set.put(std.testing.allocator, index, {});
    }

    try std.testing.expectEqual(@as(usize, 40), set.count());
    for (100..140) |raw| {
        const index: compiler.InternPool.Index = @enumFromInt(raw);
        try std.testing.expect(set.contains(index));
    }

    const removed: compiler.InternPool.Index = @enumFromInt(117);
    set.popExpected(removed);
    try std.testing.expect(!set.contains(removed));
    try std.testing.expectEqual(@as(usize, 39), set.count());

    try set.put(std.testing.allocator, removed, {});
    try std.testing.expect(set.contains(removed));
    try std.testing.expectEqual(@as(usize, 40), set.count());
}
