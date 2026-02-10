// Test that slice creation doesn't crash the analyzer
pub fn main() u8 {
    var arr: [10]u8 = undefined;
    const slice = arr[2..5];
    _ = slice;
    return 0;
}
