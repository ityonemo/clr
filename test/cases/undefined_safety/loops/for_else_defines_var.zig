// Test: variable defined in both break path and else clause - should be defined
pub fn main() u8 {
    var result: u8 = undefined;
    var arr = [_]u8{ 1, 2, 3 };
    for (&arr) |*item| {
        if (item.* == 2) {
            result = item.*;
            break;
        }
    } else {
        result = 255; // Only runs if no break
    }
    return result; // Both paths define result - should be defined!
}
