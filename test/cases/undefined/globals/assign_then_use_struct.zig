// Test that assigning to an undefined global struct field then using it works
const Point = struct {
    x: i32,
    y: i32,
};

var global_point: Point = undefined;

fn use_struct() i32 {
    global_point.x = 47;
    return global_point.x;
}

pub fn main() u8 {
    const result = use_struct();
    _ = result;
    return 0;
}
