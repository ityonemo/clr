pub fn main() u8 {
    var arr: [3]u8 = undefined;
    _ = &arr; // suppress "never mutated" warning
    return arr[0];
}
