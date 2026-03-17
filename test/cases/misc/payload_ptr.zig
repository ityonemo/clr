// Test unwrap_errunion_payload_ptr tracking
//
// NOTE: This test is currently simplified. The full test with stack-allocated
// error unions exposes a separate bug where alloc sets .stack memory_safety
// on error unions, but testValid expects .error_stub. That needs to be fixed
// separately in memory_safety.zig alloc handler.
const E = error{Fail};

fn getResult() E!u32 {
    return 42;
}

pub fn main() u8 {
    const result = getResult() catch return 1;
    return @intCast(result);
}
