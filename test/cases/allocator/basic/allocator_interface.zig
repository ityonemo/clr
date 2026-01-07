const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // === Single Item: create/destroy ===
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr);

    // === Basic Slice: alloc/free ===
    const slice = allocator.alloc(u8, 10) catch return 2;
    slice[0] = 1;
    allocator.free(slice);

    // === alignedAlloc ===
    const aligned = allocator.alignedAlloc(u8, .@"16", 10) catch return 3;
    aligned[0] = 2;
    allocator.free(aligned);

    // === allocSentinel ===
    const sentinel_slice = allocator.allocSentinel(u8, 10, 0) catch return 4;
    sentinel_slice[0] = 3;
    allocator.free(sentinel_slice);

    // === resize (returns bool) ===
    const resizable = allocator.alloc(u8, 10) catch return 5;
    resizable[0] = 4;
    _ = allocator.resize(resizable, 20);
    allocator.free(resizable);

    // === realloc ===
    var reallocable = allocator.alloc(u8, 10) catch return 6;
    reallocable[0] = 5;
    reallocable = allocator.realloc(reallocable, 20) catch return 7;
    allocator.free(reallocable);

    // === dupe ===
    const original: []const u8 = "hello";
    const duped = allocator.dupe(u8, original) catch return 8;
    allocator.free(duped);

    // === dupeZ ===
    const dupedZ = allocator.dupeZ(u8, original) catch return 9;
    allocator.free(dupedZ);

    return 0;
}
