// Switch on union inside loop - should pass
// The union variant is properly checked before access
const Value = union(enum) {
    int: i32,
    flag: bool,
};

pub fn main() u8 {
    var val: Value = .{ .int = 42 };
    _ = &val;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        switch (val) {
            .int => |n| {
                _ = n;
            },
            .flag => |f| {
                _ = f;
            },
        }
    }

    return 0;
}
