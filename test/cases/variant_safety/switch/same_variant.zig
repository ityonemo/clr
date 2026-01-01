const Value = union(enum) {
    int: i32,
    float: f32,
};

pub fn main() u8 {
    var v: Value = .{ .int = 0 };
    var selector: u8 = 0;
    _ = &selector;

    switch (selector) {
        0 => {
            v = .{ .int = 47 };
        },
        1 => {
            v = .{ .int = 100 };
        },
        else => {
            v = .{ .int = 200 };
        },
    }

    // OK: all cases set .int
    const i = v.int;
    _ = i;
    return 0;
}
