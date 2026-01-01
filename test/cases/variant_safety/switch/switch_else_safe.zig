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

    // Switch with else - else doesn't access any variants
    const result: i32 = switch (v) {
        .int => |i| i, // safe - we know it's .int here
        else => -1, // safe - no variant access
    };
    _ = result;

    return 0;
}
