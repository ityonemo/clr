// Test: for loop with index capture - both paths define variable
pub fn main() u8 {
    const arr = [_]u8{ 10, 20, 30 };
    var found_idx: u8 = undefined;
    for (arr, 0..) |item, idx| {
        if (item == 20) {
            found_idx = @intCast(idx);
            break;
        }
    } else {
        found_idx = 255; // Not found
    }
    return found_idx; // Should be 1
}
