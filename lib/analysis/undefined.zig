const std = @import("std");
const Slot = @import("../slots.zig").Slot;

pub const Meta = struct {
    file: ?[]const u8 = null,
    line: ?u32 = null,
    column: ?u32 = null,
    var_name: ?[]const u8 = null,
};

pub const Undefined = union(enum) {
    defined: void,
    undefined: Meta,

    pub fn alloc(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = payload;
        tracked[index].undefined = .{ .undefined = .{
            .file = ctx.file,
            .line = ctx.line,
            .column = ctx.column,
        } };
    }

    pub fn store_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const ptr = payload.ptr orelse return;
        if (payload.is_undef) {
            tracked[ptr].undefined = .{ .undefined = .{
                .file = ctx.file,
                .line = ctx.line,
                .column = ctx.column,
            } };
        } else {
            tracked[ptr].undefined = .{ .defined = {} };
            // Propagate defined status to caller's slot if this is an arg
            if (tracked[ptr].arg_ptr) |arg_ptr| {
                arg_ptr.undefined = .{ .defined = {} };
            }
        }
    }

    pub fn load(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const ptr = payload.ptr orelse return;
        const slot = tracked[ptr];
        if (slot.undefined) |undef| {
            switch (undef) {
                .undefined => return undef.reportUseBeforeAssign(ctx),
                .defined => {},
            }
        }
    }

    pub fn dbg_var_ptr(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        _ = ctx;
        const slot = payload.slot orelse return;
        std.debug.assert(slot < tracked.len);
        if (tracked[slot].undefined) |*undef| {
            switch (undef.*) {
                .undefined => |*meta| {
                    meta.var_name = payload.name;
                },
                .defined => {},
            }
        }
    }

    pub fn reportUseBeforeAssign(self: Undefined, ctx: anytype) error{UseBeforeAssign} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        ctx.print("use of undefined value found in {s} ({s}:{d}:{d})\n", .{ func_name, ctx.file, ctx.line, ctx.column });
        switch (self) {
            .undefined => |meta| {
                if (meta.file) |file| {
                    // Find the function where the undefined was assigned by walking the stacktrace
                    const assign_func = ctx.stacktrace.items[0];
                    if (meta.var_name) |name| {
                        ctx.print("undefined value assigned to '{s}' in {s} ({s}:{d}:{d})\n", .{
                            name,
                            assign_func,
                            file,
                            meta.line orelse 0,
                            meta.column orelse 0,
                        });
                    } else {
                        ctx.print("undefined value assigned in {s} ({s}:{d}:{d})\n", .{
                            assign_func,
                            file,
                            meta.line orelse 0,
                            meta.column orelse 0,
                        });
                    }
                }
            },
            .defined => {},
        }
        return error.UseBeforeAssign;
    }
};