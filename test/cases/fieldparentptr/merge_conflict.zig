// Test that fieldparentptr_safety detects conflicting origins after branch merge
const std = @import("std");
const S = struct { x: u8, y: u8 };

fn someCondition() bool {
    return std.crypto.random.boolean();
}

pub fn main() u8 {
    var s1: S = .{ .x = 1, .y = 2 };
    var s2: S = .{ .x = 3, .y = 4 };

    var ptr: *u8 = undefined;
    if (someCondition()) {
        ptr = &s1.x; // field_parent_ptr origin: s1, field 0
    } else {
        ptr = &s2.y; // field_parent_ptr origin: s2, field 1
    }

    // After merge, which origin should ptr have?
    const parent: *S = @fieldParentPtr("x", ptr); // Should error - ambiguous
    return parent.x;
}
