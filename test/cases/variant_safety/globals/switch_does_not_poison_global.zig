// Switching on a global union should narrow only the switch case being analyzed.
// After the switch, the global's real active variant should remain usable.
const Value = union(enum) {
    int: i32,
    float: f32,
    boolean: bool,
};

var global_union: Value = .{ .float = 3.14 };

fn inspect() void {
    switch (global_union) {
        .int => {},
        .float => {},
        .boolean => {},
    }
}

fn read_float() f32 {
    return global_union.float;
}

pub fn main() u8 {
    inspect();
    const result = read_float();
    _ = result;
    return 0;
}
