const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const EIdx = Inst.EIdx;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");

// =========================================================================
// State types
// =========================================================================

pub const StackPtr = struct {
    meta: Meta,
    name: Name = .{ .other = {} },

    pub const Name = union(enum) {
        variable: []const u8,
        parameter: []const u8,
        other: void,
    };
};

pub const Allocation = struct {
    allocated: Meta,
    freed: ?Meta = null, // null = still allocated, has value = freed
    origin: usize, // The original slot where this allocation was created
    allocator_type: []const u8, // Type of allocator that created this allocation
};

pub const MemorySafety = union(enum) {
    stack_ptr: StackPtr,
    allocation: Allocation,

    pub fn alloc(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Alloc) !void {
        _ = params;
        // Inst contains .pointer = Indirected, get the pointee
        const ptr_idx = results[index].refinement.?;
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        refinements.at(pointee_idx).scalar.memory_safety = .{ .stack_ptr = .{ .meta = ctx.meta } };
    }

    pub fn arg(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Arg) !void {
        const ptr_idx = results[index].refinement orelse return setParamStackPtr(results, index, ctx, refinements, params.name);

        // Check if caller passed a meaningful entity
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            .scalar => ptr_idx, // Non-pointer, check directly
            // Caller passed an interned constant - no meaningful entity
            .unimplemented, .unset_retval => return setParamStackPtr(results, index, ctx, refinements, params.name),
            else => return, // void, etc - nothing to check
        };

        // If the caller passed an allocation, preserve that metadata
        // so the callee can free it (ownership transfer)
        switch (refinements.at(pointee_idx).*) {
            .scalar => |imm| {
                if (imm.memory_safety) |ms| {
                    if (ms == .allocation) return; // Preserve allocation metadata
                }
            },
            else => {},
        }
        // Entity was copied from caller but doesn't have allocation - keep it as-is
    }

    fn setParamStackPtr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, name: []const u8) !void {
        // Create stack_ptr for this parameter
        // Empty function name means returning it directly won't be flagged as an escape
        _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{ .memory_safety = .{ .stack_ptr = .{
            .meta = .{
                .function = "", // Empty = not from this function's stack
                .file = ctx.meta.file,
                .line = ctx.base_line + 1, // +1 for 1-indexed line numbers
            },
            .name = .{ .parameter = name },
        } } } });
    }

    pub fn store_safe(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.StoreSafe) !void {
        _ = index;
        const ptr = params.ptr orelse return;
        const src = params.src orelse return;

        // If storing from an inst with parameter name to a stack pointer, propagate the name and meta
        const src_idx = results[src].refinement orelse return;
        const src_ms = switch (refinements.at(src_idx).*) {
            .scalar => |imm| imm.memory_safety orelse return,
            else => return,
        };
        if (src_ms != .stack_ptr) return;
        const src_sp = src_ms.stack_ptr;
        const param_name = switch (src_sp.name) {
            .parameter => |name| name,
            else => return,
        };

        // Get the target's pointee and update its stack_ptr name and meta
        const ptr_idx = results[ptr].refinement orelse return;
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            else => return,
        };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |*imm| {
                const ms = &(imm.memory_safety orelse return);
                if (ms.* != .stack_ptr) return;
                if (ms.stack_ptr.name == .other) {
                    // Copy the parameter name and update the function to the current one
                    ms.stack_ptr.name = .{ .parameter = param_name };
                    // Use the current function name and the source's file/line
                    ms.stack_ptr.meta.function = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                    ms.stack_ptr.meta.file = src_sp.meta.file;
                    ms.stack_ptr.meta.line = src_sp.meta.line;
                    ms.stack_ptr.meta.column = null; // Parameters don't have specific column
                }
            },
            else => {},
        }
    }

    pub fn dbg_var_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.DbgVarPtr) !void {
        _ = index;
        _ = ctx;
        // Set the variable name on the stack_ptr metadata
        const inst = params.slot orelse return;
        std.debug.assert(inst < results.len);
        const ptr_idx = results[inst].refinement orelse return;
        // Follow pointer to get to pointee
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            .scalar => ptr_idx, // For non-pointer types, use directly
            .unimplemented, .unset_retval, .void => return,
            else => @panic("unexpected refinement type in dbg_var_ptr (outer)"),
        };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |*imm| {
                const ms = &(imm.memory_safety orelse return);
                if (ms.* != .stack_ptr) return;
                if (ms.stack_ptr.name == .other) {
                    ms.stack_ptr.name = .{ .variable = params.name };
                }
            },
            .unimplemented => {},
            else => @panic("unexpected refinement type in dbg_var_ptr (pointee)"),
        }
    }

    pub fn bitcast(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Bitcast) !void {
        // Since we now share entities intraprocedrually, no copying needed.
        // The entity is already shared between source and destination insts.
        _ = results;
        _ = index;
        _ = ctx;
        _ = refinements;
        _ = params;
    }

    pub fn optional_payload(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.OptionalPayload) !void {
        // Since we now share entities intraprocedrually, no copying needed.
        // The entity is already shared between source and destination insts.
        _ = results;
        _ = index;
        _ = ctx;
        _ = refinements;
        _ = params;
    }

    pub fn ret_safe(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.RetSafe) !void {
        _ = index;

        const src = params.src orelse return;
        std.debug.assert(src < results.len);

        const src_idx = results[src].refinement orelse return;

        // Follow pointer to get to pointee's memory_safety
        const pointee_idx = switch (refinements.at(src_idx).*) {
            .pointer => |ind| ind.to,
            .scalar => src_idx, // Non-pointer, check directly
            .unimplemented => return,
            else => return, // void, etc - nothing to check
        };

        const ms = switch (refinements.at(pointee_idx).*) {
            .scalar => |imm| imm.memory_safety orelse return,
            else => return,
        };

        // Check for stack pointer escape
        switch (ms) {
            .stack_ptr => |sp| {
                const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                // Only flag as escape if stack_ptr is from this function
                // Args have empty function name, so they won't match
                if (std.mem.eql(u8, sp.meta.function, func_name)) {
                    return reportStackEscape(ms, ctx);
                }
            },
            .allocation => {},
        }

        // Note: Interprocedural propagation is now handled in RetSafe.apply in tag.zig
        // via caller_refinements and return_eidx. Memory safety state is copied along with
        // the Refinement when return values are propagated to the caller.
    }

    /// Called on function close to check for memory leaks.
    /// Backward propagation is handled centrally by Inst.backPropagate().
    pub fn onFinish(results: []Inst, ctx: *Context, refinements: *Refinements) !void {
        // Check for memory leaks
        for (results) |inst| {
            const idx = inst.refinement orelse continue;
            const pointee_idx = switch (refinements.at(idx).*) {
                .pointer => |ind| ind.to,
                .unimplemented, .void, .scalar => continue,
                else => continue,
            };
            const imm = switch (refinements.at(pointee_idx).*) {
                .scalar => |imm| imm,
                else => continue,
            };
            const ms = imm.memory_safety orelse continue;
            if (ms != .allocation) continue;
            const a = ms.allocation;

            if (a.freed == null) {
                if (isOriginFreed(results, refinements, a.origin)) continue;
                return reportMemoryLeak(ctx, a.allocated);
            }
        }
    }

    /// Check if any inst with the given origin has been freed
    fn isOriginFreed(results: []Inst, refinements: *Refinements, origin: usize) bool {
        for (results) |inst| {
            const idx = inst.refinement orelse continue;
            // Follow pointer to get to scalar
            const pointee_idx = switch (refinements.at(idx).*) {
                .pointer => |ind| ind.to,
                .unimplemented, .void, .scalar => continue,
                else => continue,
            };
            const imm = switch (refinements.at(pointee_idx).*) {
                .scalar => |imm| imm,
                else => continue,
            };
            const ms = imm.memory_safety orelse continue;
            if (ms != .allocation) continue;
            if (ms.allocation.origin == origin and ms.allocation.freed != null) {
                return true;
            }
        }
        return false;
    }

    // =========================================================================
    // Allocation tracking (use-after-free, double-free, memory leak detection)
    // =========================================================================

    /// Handle allocator.create() - marks pointed-to memory as allocated
    pub fn alloc_create(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.AllocCreate) !void {
        // Inst contains .pointer = Indirected, get the pointee
        const ptr_idx = results[index].refinement.?;
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        refinements.at(pointee_idx).scalar.memory_safety = .{ .allocation = .{
            .allocated = ctx.meta,
            .origin = index, // This inst is the origin of this allocation
            .allocator_type = params.allocator_type,
        } };
    }

    /// Handle allocator.destroy() - marks as freed, detects double-free and mismatched allocator
    pub fn alloc_destroy(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.AllocDestroy) !void {
        _ = index;
        const ptr = params.ptr orelse return;
        std.debug.assert(ptr < results.len);

        // Follow the pointer to get the pointee
        const ptr_idx = results[ptr].refinement orelse @panic("alloc_destroy: inst has no refinement");
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            .unimplemented => return,
            else => @panic("alloc_destroy: expected pointer type"),
        };
        const imm = switch (refinements.at(pointee_idx).*) {
            .scalar => |imm| imm,
            .unimplemented => return,
            else => @panic("alloc_destroy: pointee is not scalar"),
        };
        const ms = imm.memory_safety orelse @panic("alloc_destroy: pointee has no memory_safety");

        switch (ms) {
            .stack_ptr => |sp| return reportFreeStackMemory(ctx, sp),
            .allocation => |a| {
                if (a.freed) |previous_free| {
                    return reportDoubleFree(ctx, a.allocated, previous_free);
                }
                // Check for mismatched allocator types
                const alloc_is_generic = std.mem.eql(u8, a.allocator_type, "Allocator");
                const free_is_generic = std.mem.eql(u8, params.allocator_type, "Allocator");
                // TODO: Interprocedural - previously used arg_ptr to detect came_from_param
                const came_from_param = false;
                const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                const came_from_other_func = !std.mem.eql(u8, a.allocated.function, func_name);

                const should_check = if (came_from_param or came_from_other_func)
                    !alloc_is_generic and !free_is_generic
                else
                    !alloc_is_generic or !free_is_generic;

                if (should_check and !std.mem.eql(u8, a.allocator_type, params.allocator_type)) {
                    return reportMismatchedAllocator(ctx, a, params.allocator_type);
                }
                markAllocationFreed(results, refinements, a.origin, ctx.meta);
            },
        }
    }

    /// Mark all insts with the given origin as freed.
    fn markAllocationFreed(results: []Inst, refinements: *Refinements, origin: usize, free_meta: Meta) void {
        for (results) |inst| {
            const idx = inst.refinement orelse continue;
            // Follow pointer to get to scalar
            const pointee_idx = switch (refinements.at(idx).*) {
                .pointer => |ind| ind.to,
                .unimplemented, .void, .scalar => continue,
                else => continue,
            };
            const imm = switch (refinements.at(pointee_idx).*) {
                .scalar => |*imm| imm,
                else => continue,
            };
            const ms = &(imm.memory_safety orelse continue);
            if (ms.* != .allocation) continue;
            if (ms.allocation.origin == origin) {
                ms.allocation.freed = free_meta;
            }
        }
    }

    /// Handle load - detect use-after-free
    pub fn load(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Load) !void {
        _ = index;
        const ptr = params.ptr orelse return;
        std.debug.assert(ptr < results.len);

        // Follow the pointer to get the pointee
        const ptr_idx = results[ptr].refinement orelse return;
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            .unimplemented => return,
            else => return, // Not a pointer type, nothing to check
        };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |imm| {
                const ms = imm.memory_safety orelse return;
                if (ms != .allocation) return;
                const a = ms.allocation;
                if (a.freed) |free_site| {
                    return reportUseAfterFree(ctx, a.allocated, free_site);
                }
            },
            else => {},
        }
    }

    // =========================================================================
    // Error reporting
    // =========================================================================

    fn reportStackEscape(ms: MemorySafety, ctx: *Context) anyerror {
        const sp = ms.stack_ptr;
        try ctx.meta.print(ctx.writer, "stack pointer escape in ", .{});
        switch (sp.name) {
            .variable => |name| {
                try sp.meta.print(ctx.writer, "pointer was for local variable '{s}' in ", .{name});
            },
            .parameter => |name| {
                if (name.len > 0) {
                    try sp.meta.print(ctx.writer, "pointer was for parameter '{s}' created in ", .{name});
                } else {
                    try sp.meta.print(ctx.writer, "pointer was for parameter created in ", .{});
                }
            },
            .other => {
                try sp.meta.print(ctx.writer, "pointer was for stack memory created in ", .{});
            },
        }
        return error.StackPointerEscape;
    }

    fn reportDoubleFree(ctx: *Context, alloc_site: Meta, previous_free: Meta) anyerror {
        try ctx.meta.print(ctx.writer, "double free in ", .{});
        try previous_free.print(ctx.writer, "previously freed in ", .{});
        try alloc_site.print(ctx.writer, "originally allocated in ", .{});
        return error.DoubleFree;
    }

    fn reportUseAfterFree(ctx: *Context, alloc_site: Meta, free_site: Meta) anyerror {
        try ctx.meta.print(ctx.writer, "use after free in ", .{});
        try free_site.print(ctx.writer, "freed in ", .{});
        try alloc_site.print(ctx.writer, "allocated in ", .{});
        return error.UseAfterFree;
    }

    fn reportMemoryLeak(ctx: *Context, alloc_site: Meta) anyerror {
        try ctx.meta.print(ctx.writer, "memory leak in ", .{});
        try alloc_site.print(ctx.writer, "allocated in ", .{});
        return error.MemoryLeak;
    }

    fn reportMismatchedAllocator(ctx: *Context, allocation: Allocation, destroy_allocator: []const u8) anyerror {
        try ctx.meta.print(ctx.writer, "allocator mismatch in ", .{});
        try allocation.allocated.print(ctx.writer, "allocated with {s} in ", .{allocation.allocator_type});
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "freed with {s}\n", .{destroy_allocator}) catch return error.FormatError;
        try ctx.writer.writeAll(msg);
        return error.MismatchedAllocator;
    }

    fn reportFreeStackMemory(ctx: *Context, sp: StackPtr) anyerror {
        try ctx.meta.print(ctx.writer, "free of stack memory in ", .{});
        switch (sp.name) {
            .variable => |name| {
                try sp.meta.print(ctx.writer, "pointer is to local variable '{s}' in ", .{name});
            },
            .parameter => |name| {
                try sp.meta.print(ctx.writer, "pointer is to parameter '{s}' in ", .{name});
            },
            .other => {
                try sp.meta.print(ctx.writer, "pointer is to stack memory in ", .{});
            },
        }
        return error.FreeStackMemory;
    }
};

// =========================================================================
// Tests
// =========================================================================

/// Helper to create a test context with specific meta values
fn initTestContext(allocator: std.mem.Allocator, discarding: *std.Io.Writer.Discarding, file: []const u8, line: u32, column: ?u32, base_line: u32) Context {
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.file = file;
    ctx.meta.line = line;
    ctx.meta.column = column;
    ctx.meta.function = "test_func";
    ctx.base_line = base_line;
    return ctx;
}

test "alloc sets stack_ptr metadata" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    try MemorySafety.alloc(&results, 1, &ctx, &refinements, .{});

    const ms = refinements.at(results[1].refinement.?).scalar.memory_safety.?;
    try std.testing.expectEqualStrings("test_func", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 10), ms.stack_ptr.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), ms.stack_ptr.meta.column);
    try std.testing.expectEqual(.other, std.meta.activeTag(ms.stack_ptr.name));
}

test "arg sets stack_ptr with empty function and parameter name" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 1);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    try MemorySafety.arg(&results, 0, &ctx, &refinements, .{ .value = undefined, .name = "my_param", .caller_refinements = null });

    const ms = refinements.at(results[0].refinement.?).scalar.memory_safety.?;
    try std.testing.expectEqualStrings("", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 2), ms.stack_ptr.meta.line); // base_line + 1
    try std.testing.expectEqual(.parameter, std.meta.activeTag(ms.stack_ptr.name));
    try std.testing.expectEqualStrings("my_param", ms.stack_ptr.name.parameter);
}

test "dbg_var_ptr sets variable name when name is other" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // First alloc to set up stack_ptr with .other name
    try MemorySafety.alloc(&results, 1, &ctx, &refinements, .{});
    const ms1 = refinements.at(results[1].refinement.?).scalar.memory_safety.?;
    try std.testing.expectEqual(.other, std.meta.activeTag(ms1.stack_ptr.name));

    // dbg_var_ptr should set the variable name
    try MemorySafety.dbg_var_ptr(&results, 0, &ctx, &refinements, .{ .slot = 1, .name = "foo" });

    const ms2 = refinements.at(results[1].refinement.?).scalar.memory_safety.?;
    try std.testing.expectEqual(.variable, std.meta.activeTag(ms2.stack_ptr.name));
    try std.testing.expectEqualStrings("foo", ms2.stack_ptr.name.variable);
}

test "bitcast propagates stack_ptr metadata" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Set up source with stack_ptr
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .scalar = .{ .memory_safety = .{ .stack_ptr = .{
        .meta = .{
            .function = "source_func",
            .file = "source.zig",
            .line = 42,
            .column = 7,
        },
        .name = .{ .variable = "src_var" },
    } } } });

    try MemorySafety.bitcast(&results, 1, &ctx, &refinements, .{ .src = 0 });

    const ms = refinements.at(results[1].refinement.?).scalar.memory_safety.?;
    try std.testing.expectEqualStrings("source_func", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("source.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 42), ms.stack_ptr.meta.line);
    try std.testing.expectEqualStrings("src_var", ms.stack_ptr.name.variable);
}

test "ret_safe detects escape when returning stack pointer from same function" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Inst with stack_ptr from test_func (current function)
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .scalar = .{ .memory_safety = .{ .stack_ptr = .{
        .meta = .{
            .function = "test_func",
            .file = "test.zig",
            .line = 5,
        },
        .name = .{ .variable = "local" },
    } } } });

    try std.testing.expectError(
        error.StackPointerEscape,
        MemorySafety.ret_safe(&results, 1, &ctx, &refinements, .{ .caller_refinements = null, .return_eidx = 0, .src = 0 }),
    );
}

test "ret_safe allows returning arg (empty function name)" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Inst with empty function name (arg)
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .scalar = .{ .memory_safety = .{ .stack_ptr = .{
        .meta = .{
            .function = "",
            .file = "test.zig",
            .line = 5,
        },
        .name = .{ .parameter = "param" },
    } } } });

    // Should NOT error - returning arg is fine
    try MemorySafety.ret_safe(&results, 1, &ctx, &refinements, .{ .caller_refinements = null, .return_eidx = 0, .src = 0 });
}
