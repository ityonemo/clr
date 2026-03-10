// Test that storing a struct returned from a function call
// properly propagates defined state to the destination.
// This exercises the fix for undefined_safety.store handling structs.

const Point = struct {
    x: i32,
    y: i32,
};

fn makePoint(x: i32, y: i32) Point {
    return .{ .x = x, .y = y };
}

pub fn main() u8 {
    // Call returns a struct, which is stored to local variable
    // The store should copy the defined state from the call result
    var p = makePoint(10, 20);

    // Using the fields should NOT trigger "use of undefined value"
    // because the store should have propagated the defined state
    _ = &p;
    const sum = p.x + p.y;

    if (sum == 30) {
        return 0;
    }
    return 1;
}
