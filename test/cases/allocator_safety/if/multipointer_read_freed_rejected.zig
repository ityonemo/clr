const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const live = allocator.create(u8) catch return 1;
    const stale = allocator.create(u8) catch {
        allocator.destroy(live);
        return 2;
    };
    live.* = 1;
    stale.* = 2;
    allocator.destroy(stale);

    var choose_live = true;
    _ = &choose_live;
    const selected = if (choose_live) live else stale;
    _ = selected.*;

    allocator.destroy(live);
    return 0;
}
