// Test that returning an error doesn't crash the analyzer
const Error = error{BadValue};

fn mayFail(val: u8) Error!u8 {
    if (val > 100) {
        return error.BadValue;
    }
    return val;
}

pub fn main() u8 {
    const result = mayFail(50) catch 0;
    return result;
}
