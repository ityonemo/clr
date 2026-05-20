// Test: allocation stored through argument pointer should not be flagged as leak
// This reproduces the pattern in ArrayList.ensureTotalCapacityPrecise

const std = @import("std");

const Container = struct {
    items: []u8,
};

fn maybeAllocate(self: *Container, allocator: std.mem.Allocator, should_alloc: bool) void {
    if (should_alloc) {
        // This allocation is stored through the argument pointer
        // It should NOT be flagged as a leak at branch merge
        self.items = allocator.alloc(u8, 10) catch @panic("alloc failed");
    }
    // Implicit else: do nothing
}

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var container = Container{ .items = &.{} };

    // Condition is runtime to avoid comptime evaluation
    var should_alloc: bool = true;
    _ = &should_alloc;

    maybeAllocate(&container, allocator, should_alloc);

    // Always free - allocator handles empty slice case
    allocator.free(container.items);

    return 0;
}
