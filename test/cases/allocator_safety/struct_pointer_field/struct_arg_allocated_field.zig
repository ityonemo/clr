// Test that allocations reachable through struct arguments are not falsely reported as leaks
// at branch merge points inside called functions.
//
// Pattern: HashMap passes self to getIndex, metadata field points to allocated memory.
// During branch merges in getIndex, the allocation should still be considered reachable.
const std = @import("std");

const Container = struct {
    data: ?[*]u8,
};

fn search(self: Container) bool {
    // Extract the allocated pointer from the struct field
    const ptr = self.data orelse return false;

    // Branch that doesn't use the pointer
    if (ptr[0] == 0) {
        return false;
    }

    // Branch merge here - allocation should NOT be reported as leaked
    // because it's still reachable through 'self.data' argument
    return true;
}

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Allocate memory
    const data = allocator.alloc(u8, 10) catch return 1;
    defer allocator.free(data);

    // Initialize
    data[0] = 1;

    // Create container with allocated data
    const container = Container{ .data = data.ptr };

    // Call function that has branches
    _ = search(container);

    return 0;
}
