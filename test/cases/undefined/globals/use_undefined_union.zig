// Test that using a global union initialized to undefined is detected
const Value = union(enum) {
    int: i32,
    float: f32,
};

var global_union: Value = undefined;

fn use_union() i32 {
    return global_union.int;
}

pub fn main() u8 {
    const result = use_union();
    _ = result;
    return 0;
}
