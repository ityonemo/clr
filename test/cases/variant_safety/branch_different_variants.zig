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
        v = .{ .float = 3.14 };
    }

    // Error: accessing .int but could be .float from else branch
    const i = v.int;
    _ = i;
    return 0;
}
