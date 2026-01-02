const Point = struct {
    x: u8,
    y: u8,
};

pub fn main() u8 {
    var p: Point = undefined;
    const ptr = &p.x;
    ptr.* = 5; // Define x through pointer
    return p.x; // OK: p.x is now defined
}
