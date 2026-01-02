const Point = struct {
    x: u8,
    y: u8,
};

pub fn main() u8 {
    var p: Point = .{ .x = 5, .y = 10 };
    const ptr = &p.x; // x is defined
    return ptr.*; // OK: dereferencing pointer to defined value
}
