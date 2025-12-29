const Value = union(enum) {
    int: i32,
    float: f32,
};

pub fn main() u8 {
    var v: Value = .{ .int = 0 };
    var condition: bool = true;
    _ = &condition; // prevent comptime optimization

    if (condition) {
        v = .{ .int = 47 };
    } else {
        v = .{ .int = 99 };
    }

    // OK: both branches set .int, so .int is definitely active
    const i = v.int;
    _ = i;
    return 0;
}
