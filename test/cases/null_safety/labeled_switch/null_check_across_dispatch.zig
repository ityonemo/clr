// Test: null check in one case, use in next case
// After fix: should pass (null check propagates across dispatch)
pub fn main() u8 {
    const State = enum { check, use };
    var maybe_ptr: ?*const u8 = null;
    const value: u8 = 42;
    maybe_ptr = &value;

    state: switch (State.check) {
        .check => {
            if (maybe_ptr == null) {
                return 0; // Exit if null
            }
            continue :state .use; // Only reaches here if non-null
        },
        .use => {
            // maybe_ptr should be known non-null here
            return maybe_ptr.?.*;
        },
    }
}
