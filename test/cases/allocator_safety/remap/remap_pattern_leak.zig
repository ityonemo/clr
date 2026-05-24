// Minimal repro for stack pointer escape false positive in shrinkAndFree
const std = @import("std");

const MyArray = struct {
    items: []u8,
    capacity: usize,
};

fn remap(old: []u8, new_len: usize) ?[]u8 {
    // Simulate remap failing - return null to force alloc path
    _ = old;
    _ = new_len;
    return null;
}

fn shrinkAndFree(self: *MyArray, allocator: std.mem.Allocator, new_len: usize) void {
    const old_memory = self.items;

    if (remap(old_memory, new_len)) |new_items| {
        self.capacity = new_items.len;
        self.items = new_items;
        return;
    }

    // Allocate new memory - this gets flagged as "stack memory"
    const new_memory = allocator.alloc(u8, new_len) catch {
        self.items.len = new_len;
        return;
    };

    @memcpy(new_memory, self.items[0..new_len]);
    allocator.free(old_memory);

    // Store new allocation into self - should NOT be stack escape
    self.items = new_memory;
    self.capacity = new_memory.len;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    const initial = allocator.alloc(u8, 10) catch return 1;
    var arr = MyArray{
        .items = initial,
        .capacity = 10,
    };

    shrinkAndFree(&arr, allocator, 5);

    allocator.free(arr.items);
    return 0;
}
