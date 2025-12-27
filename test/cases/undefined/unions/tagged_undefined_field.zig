const Value = union(enum) {
    int: u8,
    float: f32,
};

pub fn main() u8 {
    var v: Value = undefined;
    v = .{ .int = undefined };
    return v.int; // Error: accessing undefined field
}
