const std = @import("std");

const QueueLike = struct {
    allocator: std.mem.Allocator,
    items: []*u8,

    fn init(allocator: std.mem.Allocator) !QueueLike {
        return .{
            .allocator = allocator,
            .items = try allocator.alloc(*u8, 4),
        };
    }

    fn deinit(self: *QueueLike) void {
        self.allocator.free(self.items);
    }
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var map = std.AutoHashMap(u32, QueueLike).init(allocator);
    defer map.deinit();

    var value = QueueLike.init(allocator) catch return 1;
    map.put(1, value) catch {
        value.deinit();
        return 1;
    };
    defer map.getPtr(1).?.deinit();
    return 0;
}
