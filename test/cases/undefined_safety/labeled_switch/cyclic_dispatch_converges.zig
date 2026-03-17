// Test: cyclic dispatch that should converge
// case A defines x, dispatches to B
// case B may dispatch back to A or exit
// After fix: should pass (x is defined on all paths to exit)
pub fn main() u8 {
    const State = enum { init, loop, exit };
    var x: u8 = undefined;
    var count: u8 = 0;

    state: switch (State.init) {
        .init => {
            x = 10; // Define x
            continue :state .loop;
        },
        .loop => {
            count += 1;
            x += 1; // x is defined (from init or previous loop iteration)
            if (count < 3) {
                continue :state .loop; // Cycle back
            }
            continue :state .exit;
        },
        .exit => {
            // x should be defined (13 = 10 + 3)
        },
    }
    return x;
}
