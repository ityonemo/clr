const Point = struct {
    x: u8,
    y: u8,
};

pub fn main() u8 {
    var p: Point = undefined;
    p.x = 5;
    // p.y is still undefined
    return p.x + p.y; // Error: p.y is undefined
}
