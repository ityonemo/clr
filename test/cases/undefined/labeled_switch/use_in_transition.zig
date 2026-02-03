// Test: use undefined value in state transition
pub fn main() u8 {
    var x: u8 = undefined;
    var counter: u8 = 0;
    state: switch (@as(u8, 0)) {
        0 => {
            counter = x; // ERROR: x is undefined
            continue :state 1;
        },
        1 => {
            x = 42;
        },
        else => {},
    }
    return counter;
}
