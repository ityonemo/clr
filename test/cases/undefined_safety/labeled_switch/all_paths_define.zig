// Test: all entry paths to final state define variable
pub fn main() u8 {
    const State = enum { path_a, path_b, final };
    var result: u8 = undefined;
    state: switch (State.path_a) {
        .path_a => {
            result = 10;
            continue :state .final;
        },
        .path_b => {
            result = 20;
            continue :state .final;
        },
        .final => {
            result += 1; // Both paths defined result before arriving here
        },
    }
    return result; // Should be defined (11 from path path_a->final)
}
