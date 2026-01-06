// Tests uniform region model: setting ANY element to undefined marks ALL as undefined
pub fn main() u8 {
    var arr: [3]u8 = .{ 1, 2, 3 }; // Start defined
    arr[1] = undefined; // Set one element to undefined
    return arr[0]; // Access different element - should error (uniform model)
}
