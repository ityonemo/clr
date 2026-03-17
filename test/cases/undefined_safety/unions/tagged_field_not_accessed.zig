const Value = union(enum) {
    int: u8,
    float: f32,
};

pub fn main() u8 {
    var v: Value = undefined;
    v = .{ .int = undefined };
    // Don't access the undefined field
    return 0;
}
