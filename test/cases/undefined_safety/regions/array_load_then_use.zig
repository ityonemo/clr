// Tests loading array element into local variable then using it
pub fn main() u8 {
    var arr: [3]u8 = undefined;
    _ = &arr;
    const val = arr[0]; // Load undefined element
    return val; // Error: val is undefined
}
