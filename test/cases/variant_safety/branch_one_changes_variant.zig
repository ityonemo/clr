const Value = union(enum) {
    int: i32,
    float: f32,
};

pub fn main() u8 {
    var v: Value = .{ .int = 42 };
    var condition: bool = true;
    _ = &condition; // prevent comptime optimization

    if (condition) {
        v = .{ .float = 3.14 };
    }
    // else: v stays as .int

    // Error: could be .int (else) or .float (then)
    const i = v.int;
    _ = i;
    return 0;
}
