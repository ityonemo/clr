const std = @import("std");

const Container = union {
    ptr: *u8,
    other: u16,
};

// Tests that freeing through @fieldParentPtr on a union is valid.
// Uses stack-allocated container to avoid Load limitation (see CLAUDE.md).
// NOTE: Simplified to avoid union load+struct_field_val issue (pre-existing bug).
pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Stack-allocated union holding a heap pointer
    var container: Container = undefined;
    container.ptr = allocator.create(u8) catch return 1;
    // Skip writing to container.ptr.* - that triggers a separate union analysis issue

    // Get a field pointer to the ptr field
    const field_ptr: **u8 = &container.ptr;

    // Use @fieldParentPtr to recover the container pointer
    const recovered: *Container = @fieldParentPtr("ptr", field_ptr);

    // Free the heap allocation through the recovered container pointer
    allocator.destroy(recovered.ptr);
    return 0;
}
