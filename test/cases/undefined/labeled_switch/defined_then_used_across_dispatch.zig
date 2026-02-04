// Test: variable defined in one case, used in next case via dispatch
// After fix: should pass (variable is defined before use)
pub fn main() u8 {
    const State = enum { define, use, done };
    var x: u8 = undefined;

    state: switch (State.define) {
        .define => {
            x = 42; // Define x
            continue :state .use;
        },
        .use => {
            x += 1; // Use x - should be defined from .define case
            continue :state .done;
        },
        .done => {},
    }
    return x; // Should be 43
}
