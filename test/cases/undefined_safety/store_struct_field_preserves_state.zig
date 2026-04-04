// Test that storing a struct into a struct field preserves the source's defined state.
// This tests the fix for the bug where Store.apply modified ptr.to BEFORE splat,
// causing undefined_safety.store to copy from source to source (no-op).

const OuterStruct = struct {
    inner: InnerStruct,
};

const InnerStruct = struct {
    value: u32,
};

fn getInner() InnerStruct {
    return InnerStruct{ .value = 42 };
}

fn returnOuter() OuterStruct {
    const inner = getInner();
    // Store inner (which is defined) into result.inner
    // The bug was: undefined_safety saw ptr.to pointing to source instead of dest
    return .{ .inner = inner };
}

pub fn main() void {
    const outer = returnOuter();
    // If the bug exists, outer.inner.value would appear undefined
    _ = outer.inner.value;
}
