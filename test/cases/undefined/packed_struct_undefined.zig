// Test case for detecting truly undefined packed struct field usage.
// This should report an error because the field is never assigned.

const Flags = packed struct {
    read: bool,
    write: bool,
    execute: bool,
    padding: u5,
};

pub fn main() u8 {
    var flags: Flags = undefined;
    _ = &flags;

    // Reading a field that was never assigned - should error
    if (flags.write) {
        return 1;
    }
    return 0;
}
