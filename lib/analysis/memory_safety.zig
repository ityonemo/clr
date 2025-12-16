
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
        if (slot >= tracked.len) @panic("improper slot value");
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
        if (src >= tracked.len) return;
        if (tracked[src].memory_safety) |ms| {
            tracked[index].memory_safety = ms;
        }
    }

    pub fn store_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        _ = ctx;
        const ptr = payload.ptr orelse return;
        const src = payload.src orelse return;
        if (ptr >= tracked.len or src >= tracked.len) return;

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
        if (src >= tracked.len) return;

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
