const Value = union(enum) {
    int: i32,
    float: f32,
    string: []const u8,
};

pub fn main() u8 {
    var v: Value = .{ .int = 42 };
    var condition: bool = true;
    _ = &condition;

    if (condition) {
        v = .{ .float = 3.14 };
    }

    // Switch with else - accessing variant in else is unsafe
    switch (v) {
        .int => |i| {
            _ = i; // safe - we know it's .int here
        },
        else => {
            // Unsafe - could be .float or .string, but we try to access .float
            _ = v.float;
        },
    }

    return 0;
}
