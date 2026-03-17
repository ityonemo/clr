// Test: use undefined value in state transition
pub fn main() u8 {
    const State = enum { start, end };
    var x: u8 = undefined;
    var counter: u8 = 0;
    state: switch (State.start) {
        .start => {
            counter = x; // ERROR: x is undefined
            continue :state .end;
        },
        .end => {
            x = 42;
        },
    }
    return counter;
}
