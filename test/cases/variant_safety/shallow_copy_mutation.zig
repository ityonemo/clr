// Test that shallow-copied active_metas cannot be mutated unexpectedly
const std = @import("std");
const Union = union(enum) { a: u8, b: u8 };

fn someCondition() bool {
    return std.crypto.random.boolean();
}

pub fn main() u8 {
    var u: Union = .{ .a = 42 };

    if (someCondition()) {
        u = .{ .b = 10 };
    }
    // After merge, if shallow copy happened, both branches could see mutation
    return u.a; // Should detect ambiguous variant
}
