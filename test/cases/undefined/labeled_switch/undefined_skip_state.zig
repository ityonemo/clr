// Test: state machine where skipping a state leaves variable undefined
pub fn main() u8 {
    const State = enum { start, define, final };
    var result: u8 = undefined;
    state: switch (State.start) {
        .start => {
            // Skip define state where result would be defined
            continue :state .final;
        },
        .define => {
            result = 42;
            continue :state .final;
        },
        .final => {
            // result is undefined if we came from start
        },
    }
    return result; // ERROR: may be undefined (path start->final skips definition)
}
