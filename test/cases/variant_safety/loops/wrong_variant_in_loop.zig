// Access wrong variant inside loop - should error
// The union is set to .int but we access .flag
const Value = union(enum) {
    int: i32,
    flag: bool,
};

pub fn main() u8 {
    var val: Value = .{ .int = 42 };
    _ = &val;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        const f = val.flag; // Error: accessing .flag when .int is active
        _ = f;
    }

    return 0;
}
