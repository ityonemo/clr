// Tests array of structs - setting one element defines all (uniform model)
const Point = struct {
    x: u8,
    y: u8,
};

pub fn main() u8 {
    var points: [3]Point = undefined;
    points[1] = .{ .x = 10, .y = 20 };
    return points[0].x; // OK: uniform model - setting any element defines all
}
