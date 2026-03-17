// Test returning a struct
const MyStruct = struct {
    value: u32,
};

pub fn getValue() MyStruct {
    return .{ .value = 42 };
}

pub fn main() u8 {
    const s = getValue();
    if (s.value != 42) return 1;
    return 0;
}
