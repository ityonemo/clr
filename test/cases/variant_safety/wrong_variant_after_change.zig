const Value = union (enum) {
    int: i32,
    float: f32,
};

pub fn main() u8 {
    var v: Value = .{ .int = 42 };
    v = .{ .float = 3.14 };
    // Error: accessing .int when .float is now active
    const i = v.int;
    _ = i;
    return 0;
}
