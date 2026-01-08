// Test: Assigning through a global pointer should mark the target global as defined
// This tests that global pointer relationships are properly tracked.

var x: u8 = undefined;
var ptr: *u8 = &x;

pub fn main() u8 {
    // Assign through the global pointer - this should mark x as defined
    ptr.* = 42;

    // Using x should NOT error because we assigned through ptr
    return x;
}
