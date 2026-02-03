// Test: state machine where skipping a state leaves variable undefined
pub fn main() u8 {
    var result: u8 = undefined;
    state: switch (@as(u8, 0)) {
        0 => {
            // Skip state 1 where result would be defined
            continue :state 2;
        },
        1 => {
            result = 42;
            continue :state 2;
        },
        2 => {
            // result is undefined if we came from state 0
        },
        else => {},
    }
    return result; // ERROR: may be undefined (path 0->2 skips definition)
}
