// Test aggregate initialization for structs with runtime values
const Point = struct {
    x: u32,
    y: u32,
};

fn getValue() u32 {
    return 10;
}

pub fn main() u8 {
    const val = getValue();
    const p: Point = .{ .x = val, .y = val + 1 };
    return @intCast(p.x + p.y);
}
