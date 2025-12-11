const clr_allocator = @import("allocator.zig");

// =============================================================================
// Code Generation Templates
// =============================================================================
//
// Functions that generate Zig source code from AIR. Each function returns
// a heap-allocated string (via clr_allocator.allocPrint) or null on failure.
// =============================================================================

/// Generate a function stub
pub fn functionStub(func_index: u32, fqn: []const u8) ?[]u8 {
    return clr_allocator.allocPrint(
        \\fn fn_{d}() void {{
        \\    // function: {s}
        \\}}
        \\
    , .{ func_index, fqn });
}

/// Generate main function that calls entrypoint
pub fn mainFunction(entrypoint_index: u32) ?[]u8 {
    return clr_allocator.allocPrint(
        \\pub fn main() void {{
        \\    fn_{d}();
        \\}}
        \\
    , .{entrypoint_index});
}
