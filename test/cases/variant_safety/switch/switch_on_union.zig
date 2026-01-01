const Value = union(enum) {
    int: i32,
    float: f32,
};

pub fn main() u8 {
    var v: Value = .{ .int = 42 };
    var condition: bool = true;
    _ = &condition;

    if (condition) {
        v = .{ .float = 3.14 };
    }

    // Switch on the union tag and access correct variant in each case
    switch (v) {
        .int => |i| {
            _ = i; // safe - we know it's .int here
        },
        .float => |f| {
            _ = f; // safe - we know it's .float here
        },
    }

    return 0;
}
