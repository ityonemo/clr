const Allocator = @import("std").mem.Allocator;

pub fn Type(comptime Meta: type) type {
    return union(enum) {
        immediate: Meta,
        pointer: *Type(Meta),
        @"struct": void, // temporary. Will be a slice.
        @"union": void, // temporary. Will be a slice.

        pub fn deinit(self: Type(Meta), allocator: Allocator) void {
            switch (self) {
                .pointer => |p| {
                    p.deinit(allocator);
                    allocator.destroy(p);
                },
                // need to implement "@struct" and "@union"
                else => {},
            }
        }
    };
}
