// Tests multiple assignments - last write wins in uniform model
pub fn main() u8 {
    var arr: [3]u8 = undefined;
    arr[0] = 10; // Define all elements
    arr[1] = undefined; // Make all elements undefined again
    return arr[2]; // Error: all elements are now undefined
}
