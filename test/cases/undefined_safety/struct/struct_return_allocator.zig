const std = @import("std");

const MyStruct = struct {
    items: []u8,
    capacity: usize,
    allocator: std.mem.Allocator,

    pub fn init(gpa: std.mem.Allocator) MyStruct {
        return .{
            .items = &.{},
            .capacity = 0,
            .allocator = gpa,
        };
    }

    pub fn useAllocator(self: *MyStruct) void {
        // Access allocator through pointer - triggers load
        _ = self.allocator;
    }
};

pub fn main() u8 {
    var s = MyStruct.init(std.heap.page_allocator);
    s.useAllocator();
    return 0;
}
