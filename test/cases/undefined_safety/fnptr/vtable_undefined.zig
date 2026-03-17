const VTable = struct {
    process: *const fn (u8) u8,
};

fn uses_value(val: u8) u8 {
    return val + 1;  // Uses undefined
}

fn ignores_value(val: u8) u8 {
    _ = val;
    return 42;
}

pub fn main() u8 {
    var x: u8 = undefined;
    _ = &x;

    const cond = true;
    const vtable: VTable = if (cond) .{ .process = &uses_value } else .{ .process = &ignores_value };
    return vtable.process(x);  // Fnptr inside struct - one branch uses undefined
}
