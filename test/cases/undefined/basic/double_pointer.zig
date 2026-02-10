// Double pointer tracking test
//
// This test verifies that undefined tracking works correctly through
// double pointer indirection. When loading a pointer from a pointer-to-pointer
// and storing through the result, the original value should be tracked as defined.
//
// The key is that semideepCopy for pointers preserves the .to reference,
// so the loaded pointer still points to the same underlying value.

pub fn main() u8 {
    var x: u8 = undefined;
    var ptr: *u8 = &x;
    const ptr_ptr: **u8 = &ptr;
    const inner = ptr_ptr.*; // Load creates new pointer, but .to still references x
    inner.* = 5; // Store through inner defines x
    return x; // x is defined - should pass without error
}
