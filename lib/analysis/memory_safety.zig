
const std = @import("std");
const Slot = @import("../slots.zig").Slot;

pub const MemorySafety = union(enum) {
    stack_ptr: StackPtrMeta,

    pub const StackPtrMeta = struct {
        function: []const u8,
        file: []const u8,
        line: u32,
        column: ?u32 = null,
        name: Name = .{ .other = {} },

        pub const Name = union(enum) {
            variable: []const u8,
            parameter: []const u8,
            other: void,
        };
    };

    pub fn alloc(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = payload;
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        tracked[index].memory_safety = .{ .stack_ptr = .{
            .function = func_name,
            .file = ctx.file,
            .line = ctx.line,
            .column = ctx.column,
        } };
    }

    pub fn arg(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        // Store parameter info with empty function name - this means returning it directly
        // won't be flagged as an escape (function won't match in ret_safe)
        tracked[index].memory_safety = .{ .stack_ptr = .{
            .function = "", // Empty = not from this function's stack
            .file = ctx.file,
            .line = ctx.base_line,
            .name = .{ .parameter = payload.name },
        } };
    }

    pub fn dbg_var_ptr(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        _ = ctx;
        // Set the variable name on the stack_ptr metadata
        const slot = payload.slot orelse return;
        std.debug.assert(slot < tracked.len);
        if (tracked[slot].memory_safety) |*ms| {
            if (ms.stack_ptr.name == .other) {
                ms.stack_ptr.name = .{.variable = payload.name};
            }
        }
    }

    pub fn bitcast(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = ctx;
        // Bitcast just reinterprets the pointer type (e.g., *u8 -> *const u8)
        // Propagate stack_ptr metadata from source to result
        const src = payload.src orelse return;
        std.debug.assert(src < tracked.len);
        if (tracked[src].memory_safety) |ms| {
            tracked[index].memory_safety = ms;
        }
    }

    pub fn store_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        _ = ctx;
        const ptr = payload.ptr orelse return;
        const src = payload.src orelse return;
        std.debug.assert(ptr < tracked.len);
        std.debug.assert(src < tracked.len);

        // Check if storing from an arg slot to an alloc slot (parameter case)
        // Transfer the arg's param name/line to the alloc's stack_ptr
        if (tracked[src].reference_arg == null) return;
        const src_ms = tracked[src].memory_safety orelse return;
        if (tracked[ptr].memory_safety) |*dst_ms| {
            // Keep dst's function (the current function) so it's detected as escape
            dst_ms.stack_ptr.name = src_ms.stack_ptr.name;
            dst_ms.stack_ptr.line = src_ms.stack_ptr.line;
            dst_ms.stack_ptr.column = null;
        }
    }

    pub fn ret_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const src = payload.src orelse return;
        std.debug.assert(src < tracked.len);

        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        if (tracked[src].memory_safety) |ms| {
            // Only flag as escape if stack_ptr is from this function
            // Args have empty function name, so they won't match
            if (std.mem.eql(u8, ms.stack_ptr.function, func_name)) {
                return ms.reportStackEscape(ctx);
            }
        }
    }

    pub fn reportStackEscape(self: MemorySafety, ctx: anytype) error{StackPointerEscape} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        const meta = self.stack_ptr;
        ctx.print("stack pointer escape in {s} ({s}:{d}:{d})\n", .{ func_name, ctx.file, ctx.line, ctx.column });
        switch (meta.name) {
            .variable => |name| {
                ctx.print("pointer was for local variable '{s}' ({s}:{d}:{d})\n", .{
                    name,
                    meta.file,
                    meta.line,
                    meta.column orelse 0,
                });
            },
            .parameter => |name| {
                if (name.len > 0) {
                    ctx.print("pointer was for parameter '{s}' created in {s} ({s}:{d})\n", .{
                        name,
                        meta.function,
                        meta.file,
                        meta.line + 1, // Convert 0-indexed to 1-indexed
                    });
                } else {
                    ctx.print("pointer was for parameter created in {s} ({s}:{d})\n", .{
                        meta.function,
                        meta.file,
                        meta.line + 1, // Convert 0-indexed to 1-indexed
                    });
                }
            },
            .other => {
                ctx.print("pointer was for stack memory created in {s} ({s}:{d}:{d})\n", .{
                    meta.function,
                    meta.file,
                    meta.line,
                    meta.column orelse 0,
                });
            },
        }
        return error.StackPointerEscape;
    }
};

// Mock context for testing
const MockContext = struct {
    line: u32 = 10,
    column: u32 = 5,
    base_line: u32 = 1,
    file: []const u8 = "test.zig",
    stacktrace: std.ArrayList([]const u8),
    output: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) MockContext {
        var ctx = MockContext{
            .stacktrace = std.ArrayList([]const u8).init(allocator),
            .output = std.ArrayList(u8).init(allocator),
        };
        ctx.stacktrace.append("test_func") catch unreachable;
        return ctx;
    }

    pub fn deinit(self: *MockContext) void {
        self.stacktrace.deinit();
        self.output.deinit();
    }

    pub fn print(self: *MockContext, comptime fmt: []const u8, args: anytype) void {
        std.fmt.format(self.output.writer(), fmt, args) catch unreachable;
    }
};

test "alloc sets stack_ptr metadata" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    try MemorySafety.alloc(&tracked, 1, &ctx, .{});

    const ms = tracked[1].memory_safety.?;
    try std.testing.expectEqualStrings("test_func", ms.stack_ptr.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.file);
    try std.testing.expectEqual(@as(u32, 10), ms.stack_ptr.line);
    try std.testing.expectEqual(@as(?u32, 5), ms.stack_ptr.column);
    try std.testing.expectEqual(.other, std.meta.activeTag(ms.stack_ptr.name));
}

test "arg sets stack_ptr with empty function and parameter name" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    try MemorySafety.arg(&tracked, 0, &ctx, .{ .name = "my_param" });

    const ms = tracked[0].memory_safety.?;
    try std.testing.expectEqualStrings("", ms.stack_ptr.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.file);
    try std.testing.expectEqual(@as(u32, 1), ms.stack_ptr.line); // base_line
    try std.testing.expectEqual(.parameter, std.meta.activeTag(ms.stack_ptr.name));
    try std.testing.expectEqualStrings("my_param", ms.stack_ptr.name.parameter);
}

test "dbg_var_ptr sets variable name when name is other" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // First alloc to set up stack_ptr with .other name
    try MemorySafety.alloc(&tracked, 1, &ctx, .{});
    try std.testing.expectEqual(.other, std.meta.activeTag(tracked[1].memory_safety.?.stack_ptr.name));

    // dbg_var_ptr should set the variable name
    try MemorySafety.dbg_var_ptr(&tracked, 0, &ctx, .{ .slot = 1, .name = "foo" });

    try std.testing.expectEqual(.variable, std.meta.activeTag(tracked[1].memory_safety.?.stack_ptr.name));
    try std.testing.expectEqualStrings("foo", tracked[1].memory_safety.?.stack_ptr.name.variable);
}

test "dbg_var_ptr does not overwrite existing variable name" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Set up stack_ptr with existing variable name
    tracked[1].memory_safety = .{ .stack_ptr = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 5,
        .name = .{ .variable = "existing" },
    } };

    // dbg_var_ptr should NOT overwrite
    try MemorySafety.dbg_var_ptr(&tracked, 0, &ctx, .{ .slot = 1, .name = "new_name" });

    try std.testing.expectEqualStrings("existing", tracked[1].memory_safety.?.stack_ptr.name.variable);
}

test "dbg_var_ptr does not overwrite parameter name" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Set up stack_ptr with parameter name
    tracked[1].memory_safety = .{ .stack_ptr = .{
        .function = "",
        .file = "test.zig",
        .line = 5,
        .name = .{ .parameter = "param" },
    } };

    // dbg_var_ptr should NOT overwrite
    try MemorySafety.dbg_var_ptr(&tracked, 0, &ctx, .{ .slot = 1, .name = "var_name" });

    try std.testing.expectEqual(.parameter, std.meta.activeTag(tracked[1].memory_safety.?.stack_ptr.name));
    try std.testing.expectEqualStrings("param", tracked[1].memory_safety.?.stack_ptr.name.parameter);
}

test "bitcast propagates stack_ptr metadata" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Set up source with stack_ptr
    tracked[0].memory_safety = .{ .stack_ptr = .{
        .function = "source_func",
        .file = "source.zig",
        .line = 42,
        .column = 7,
        .name = .{ .variable = "src_var" },
    } };

    try MemorySafety.bitcast(&tracked, 1, &ctx, .{ .src = 0 });

    const ms = tracked[1].memory_safety.?;
    try std.testing.expectEqualStrings("source_func", ms.stack_ptr.function);
    try std.testing.expectEqualStrings("source.zig", ms.stack_ptr.file);
    try std.testing.expectEqual(@as(u32, 42), ms.stack_ptr.line);
    try std.testing.expectEqualStrings("src_var", ms.stack_ptr.name.variable);
}

test "store_safe transfers param name from arg to alloc" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Slot 0: arg with parameter name
    tracked[0].memory_safety = .{ .stack_ptr = .{
        .function = "",
        .file = "test.zig",
        .line = 5,
        .name = .{ .parameter = "my_param" },
    } };
    tracked[0].reference_arg = 0; // Mark as arg

    // Slot 1: alloc with current function
    tracked[1].memory_safety = .{ .stack_ptr = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 10,
        .column = 3,
        .name = .{ .other = {} },
    } };

    // store from arg (0) to alloc (1)
    try MemorySafety.store_safe(&tracked, 2, &ctx, .{ .ptr = 1, .src = 0 });

    const ms = tracked[1].memory_safety.?;
    // Function should remain (so escape detection still works)
    try std.testing.expectEqualStrings("test_func", ms.stack_ptr.function);
    // Name should be transferred from arg
    try std.testing.expectEqual(.parameter, std.meta.activeTag(ms.stack_ptr.name));
    try std.testing.expectEqualStrings("my_param", ms.stack_ptr.name.parameter);
    // Line should be transferred
    try std.testing.expectEqual(@as(u32, 5), ms.stack_ptr.line);
    // Column should be null after transfer
    try std.testing.expectEqual(@as(?u32, null), ms.stack_ptr.column);
}

test "ret_safe detects escape when returning stack pointer from same function" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Slot with stack_ptr from test_func (current function)
    tracked[0].memory_safety = .{ .stack_ptr = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 5,
        .name = .{ .variable = "local" },
    } };

    try std.testing.expectError(
        error.StackPointerEscape,
        MemorySafety.ret_safe(&tracked, 1, &ctx, .{ .src = 0 }),
    );
}

test "ret_safe allows returning arg (empty function name)" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Slot with empty function name (arg)
    tracked[0].memory_safety = .{ .stack_ptr = .{
        .function = "",
        .file = "test.zig",
        .line = 5,
        .name = .{ .parameter = "param" },
    } };

    // Should NOT error - returning arg is fine
    try MemorySafety.ret_safe(&tracked, 1, &ctx, .{ .src = 0 });
}

test "ret_safe allows returning pointer from different function" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Slot with stack_ptr from different function
    tracked[0].memory_safety = .{ .stack_ptr = .{
        .function = "other_func",
        .file = "test.zig",
        .line = 5,
    } };

    // Should NOT error - pointer is from a different function
    try MemorySafety.ret_safe(&tracked, 1, &ctx, .{ .src = 0 });
}

test "reportStackEscape formats variable name correctly" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    const ms = MemorySafety{ .stack_ptr = .{
        .function = "my_func",
        .file = "file.zig",
        .line = 42,
        .column = 8,
        .name = .{ .variable = "my_var" },
    } };

    _ = ms.reportStackEscape(&ctx) catch {};

    const output = ctx.output.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "stack pointer escape in test_func") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "pointer was for local variable 'my_var'") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "file.zig:42:8") != null);
}

test "reportStackEscape formats parameter name correctly" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    const ms = MemorySafety{ .stack_ptr = .{
        .function = "caller_func",
        .file = "file.zig",
        .line = 10, // 0-indexed
        .name = .{ .parameter = "my_param" },
    } };

    _ = ms.reportStackEscape(&ctx) catch {};

    const output = ctx.output.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "pointer was for parameter 'my_param'") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "file.zig:11") != null); // 1-indexed
}
