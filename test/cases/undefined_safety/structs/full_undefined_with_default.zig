const Point = struct {
    x: u8 = 10, // has default value
    y: u8, // no default
};

pub fn main() u8 {
    var p: Point = undefined;
    p.y = 3;
    return p.x + p.y; // x is still undefined - default not applied with = undefined
}
