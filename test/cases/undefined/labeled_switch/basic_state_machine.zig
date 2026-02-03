// Test: basic labeled switch state machine - variable defined across all states
pub fn main() u8 {
    var result: u8 = undefined;
    state: switch (@as(u8, 0)) {
        0 => {
            result = 10;
            continue :state 1;
        },
        1 => {
            result += 5;
            continue :state 2;
        },
        2 => {
            result += 1;
        },
        else => {},
    }
    return result; // Should be 16
}
