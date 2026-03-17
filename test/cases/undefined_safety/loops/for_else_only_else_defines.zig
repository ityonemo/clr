// Test: variable defined ONLY in else clause - break path leaves it undefined
pub fn main() u8 {
    var result: u8 = undefined;
    var arr = [_]u8{ 1, 2, 3 };
    for (&arr) |*item| {
        if (item.* == 2) break;
    } else {
        result = 255;
    }
    return result; // ERROR: may be undefined if break taken
}
