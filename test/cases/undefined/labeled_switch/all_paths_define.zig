// Test: all entry paths to final state define variable
pub fn main() u8 {
    var result: u8 = undefined;
    state: switch (@as(u8, 0)) {
        0 => {
            result = 10;
            continue :state 2;
        },
        1 => {
            result = 20;
            continue :state 2;
        },
        2 => {
            result += 1; // Both paths defined result before arriving here
        },
        else => {
            result = 0;
        },
    }
    return result; // Should be defined (11 from path 0->2)
}
