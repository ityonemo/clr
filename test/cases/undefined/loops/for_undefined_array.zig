// Test: for loop over undefined array - elements should be undefined
pub fn main() u8 {
    var arr: [3]u8 = undefined;
    arr[0] = 1; // Only partially defined
    var sum: u8 = 0;
    for (arr) |item| {
        sum += item; // ERROR: arr[1] and arr[2] are undefined
    }
    return sum;
}
