const Point = struct {
    x: u8,
    y: u8,
};

pub fn main() u8 {
    var p: Point = undefined;
    const ptr = &p.x; // x is undefined
    return ptr.*; // Error: dereferencing pointer to undefined value
}
