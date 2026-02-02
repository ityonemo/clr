// Variant changes in loop then accessed after - wrong variant
// After loop runs, variant is .flag but we access .int
const Value = union(enum) {
    int: u8,
    flag: bool,
};

pub fn main() u8 {
    var val: Value = .{ .int = 42 };
    _ = &val;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        // Unconditionally set to different variant
        val = .{ .flag = true };
    }

    return val.int; // Error: accessing .int when .flag is active
}
