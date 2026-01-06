// Tests array of structs - accessing field of undefined struct element
const Point = struct {
    x: u8,
    y: u8,
};

pub fn main() u8 {
    var points: [3]Point = undefined;
    _ = &points;
    return points[0].x; // Error: struct element is undefined
}
