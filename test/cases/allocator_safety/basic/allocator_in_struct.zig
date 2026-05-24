const std = @import("std");

/// Simple struct that stores an allocator - common Zig pattern.
const Container = struct {
    allocator: std.mem.Allocator,
    data: u32,

    pub fn init(allocator: std.mem.Allocator) Container {
        return Container{
            .allocator = allocator,
            .data = 42,
        };
    }
};

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const container = Container.init(allocator);
    _ = container;

    return 0;
}
