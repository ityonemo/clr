// Tests that a properly initialized array doesn't trigger undefined errors
pub fn main() u8 {
    var arr: [3]u8 = .{ 1, 2, 3 };
    _ = &arr;
    return arr[0];
}
