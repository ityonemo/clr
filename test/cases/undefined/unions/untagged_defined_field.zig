const Value = union {
    int: u8,
    float: f32,
};

pub fn main() u8 {
    var v: Value = undefined;
    v = .{ .int = 42 };
    return v.int; // OK: field is defined
}
