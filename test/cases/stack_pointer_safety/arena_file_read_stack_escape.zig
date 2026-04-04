// Minimal repro for stack pointer escape false positive
// Matches vendor wrapper pattern: ArenaAllocator + file read
const std = @import("std");

pub fn main() u8 {
    // Use arena allocator like vendor wrapper
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Read file - internally uses ArrayList which calls shrinkAndFree
    const file = std.fs.cwd().openFile("/etc/hostname", .{}) catch return 1;
    defer file.close();

    // readToEndAlloc uses ArrayList internally, triggers shrinkAndFree
    const contents = file.readToEndAlloc(allocator, 1024) catch return 1;
    _ = contents;

    return 0;
}
