// Test that switch_br preserves known variant state when exploring branches.
// When we statically know variant is .b, accessing .a in dead code should still error.
const Union = union(enum) { a: u8, b: u8 };

pub fn main() u8 {
    var u: Union = .{ .a = 42 };
    u = .{ .b = 10 }; // set_union_tag sets variant to .b - this is KNOWN

    switch (u) {
        // Even though this branch is dead code at runtime (variant is .b),
        // the analyzer explores all branches. Since .b is known active,
        // accessing .a should error (inactive variant access).
        .a => return u.a,
        .b => return u.b,
    }
}
