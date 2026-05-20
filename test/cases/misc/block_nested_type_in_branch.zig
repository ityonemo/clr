// Test that block with nested type inside a branch doesn't cause
// "placeholder in merge" error. This was a bug where Block didn't
// call splatInitDefined on nested types, leaving them with .placeholder
// memory_safety that caused panics at branch merge.

fn getSlice(cond: bool) []const u8 {
    // This creates a block with pointer→region→scalar type
    // inside the conditional, the result flows through merge
    return if (cond) "hello" else "world";
}

pub fn main() u8 {
    // Create runtime bool to prevent comptime evaluation
    var b: bool = true;
    _ = &b;

    const slice = getSlice(b);

    // Use the slice to prevent optimization
    return if (slice.len > 0) 0 else 1;
}
