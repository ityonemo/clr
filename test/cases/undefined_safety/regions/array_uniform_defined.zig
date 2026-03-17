// Tests uniform region model: setting ANY element marks ALL elements as defined
pub fn main() u8 {
    var arr: [3]u8 = undefined;
    arr[1] = 42; // Set element 1
    return arr[0]; // Access element 0 - should NOT error (uniform model)
}
