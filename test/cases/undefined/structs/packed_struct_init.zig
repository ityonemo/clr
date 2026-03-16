// Test case for packed struct initialization with block bodies.
// Packed structs use read-modify-write patterns that require block body
// ordering to work correctly in full (main) functions.

const Flags = packed struct {
    read: bool = false,
    write: bool = false,
    execute: bool = false,
    padding: u5 = 0,
};

fn getMode() u2 {
    var x: u2 = 1;
    _ = &x;
    return x;
}

pub fn main() u8 {
    const mode = getMode();

    // This switch expression initializes a packed struct.
    // Each case sets different flags using block bodies that need
    // to execute inline with their parent block.
    var flags: Flags = switch (mode) {
        0 => .{ .read = true },
        1 => .{ .write = true },
        2 => .{ .execute = true },
        3 => .{ .read = true, .write = true },
    };

    _ = &flags;

    // Use the value to prevent optimization
    if (flags.write) {
        return 0;
    }
    return 1;
}
