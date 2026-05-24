// Test case: packed struct safety must survive scalarization across a call.
// A packed struct can be bitcast to its backing integer representation and then
// unpacked later. Undefined field state should not disappear across that scalar
// boundary.

const Flags = packed struct {
    read: bool,
    write: bool,
    execute: bool,
    padding: u5,
};

fn unpackAndRead(raw: u8) u8 {
    const flags: Flags = @bitCast(raw);
    if (flags.write) {
        return 1;
    }
    return 0;
}

pub fn main() u8 {
    var flags: Flags = undefined;
    _ = &flags;

    flags.read = true;
    flags.execute = false;

    const raw: u8 = @bitCast(flags);
    return unpackAndRead(raw);
}
