const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Context = @import("../Context.zig");
const core = @import("../core.zig");
const State = @import("../lib.zig").State;
const tag = @import("../tag.zig");
const Gid = Refinements.Gid;
const MemorySafety = @import("memory_safety.zig").MemorySafety;

var test_buf: [4096]u8 = undefined;
var test_discarding = std.Io.Writer.Discarding.init(&test_buf);

fn initTest() struct { Context, Refinements } {
    const allocator = std.testing.allocator;
    var ctx = Context.init(allocator, &test_discarding.writer);
    ctx.meta.function = "test_func";
    return .{ ctx, Refinements.init(allocator) };
}

fn testState(ctx: *Context, results: []Inst, refinements: *Refinements) State {
    return .{
        .ctx = ctx,
        .results = results,
        .refinements = refinements,
        .return_gid = 0,
        .restrict = .memory_safety,
    };
}

test "alloc sets stack metadata on pointee" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Alloc creates a stack pointer
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // Check pointer has stack metadata
    const ptr_gid = results[0].refinement.?;
    const ms = refinements.at(ptr_gid).pointer.analyte.memory_safety.?;
    try std.testing.expect(ms == .stack);
}

test "set_union_tag initializes pointer field targets as placeholders" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    const union_type = tag.Type{ .@"union" = &.{
        .type_id = 100,
        .variants = &.{
            .{ .scalar = {} },
            .{ .pointer = &.{ .@"union" = &.{
                .type_id = 100,
                .variants = &.{ .{ .scalar = {} }, .{ .pointer = &.{ .recursive = 100 } } },
            } } },
        },
    } };

    try Inst.apply(state, 0, .{ .alloc = .{ .ty = union_type } });
    try Inst.apply(state, 1, .{ .set_union_tag = .{
        .ptr = .{ .inst = 0 },
        .field_index = 1,
        .ty = union_type.@"union".variants[1],
    } });
    try Inst.apply(state, 2, .{ .struct_field_ptr = .{
        .base = .{ .inst = 0 },
        .field_index = 1,
        .ty = .{ .pointer = &union_type.@"union".variants[1] },
        .type_id = 100,
    } });

    const ptr_gid = results[2].refinement.?;
    const field_gid = refinements.at(ptr_gid).pointer.to;
    const nested_union_gid = refinements.at(field_gid).pointer.to;
    try std.testing.expect(refinements.at(nested_union_gid).@"union".analyte.memory_safety != null);
}

test "init_global initializes undefined pointer field targets as placeholders" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const target_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const ptr_field_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = target_gid,
        .analyte = .{ .memory_safety = .{ .placeholder = {} } },
    } });
    const fields = try ctx.allocator.dupe(Gid, &.{ptr_field_gid});
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = fields,
        .type_id = 200,
    } });
    const global_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = struct_gid } });

    MemorySafety.init_global(&refinements, global_ptr_gid, struct_gid, &ctx, false, false, .{ .file = "test.zig", .line = 1, .column = 1 }, null);

    try std.testing.expect(refinements.at(target_gid).scalar.analyte.memory_safety != null);
    try std.testing.expectEqual(.placeholder, std.meta.activeTag(refinements.at(target_gid).scalar.analyte.memory_safety.?));
}

test "call intercepts mem.Allocator.create and sets allocation metadata" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create allocator refinement (the allocator argument)
    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = alloc_gid; // allocator at inst 0

    const state = testState(&ctx, &results, &refinements);

    // Call mem.Allocator.create - Inst.call creates the return structure
    const scalar_type: tag.Type = .{ .scalar = {} };
    const ptr_type: tag.Type = .{ .pointer = &scalar_type };
    const return_type: tag.Type = .{ .errorunion = &ptr_type };
    const args = &[_]tag.Src{.{ .inst = 0 }}; // allocator arg
    try Inst.call(state, 1, null, return_type, args, "std.mem.Allocator.create");

    // Check the result was created and POINTEE has allocation metadata
    // Note: memory_safety is set on the POINTEE, not the pointer itself.
    // The pointer is a return value (register/stack), the pointee is on the heap.
    const eu_gid = results[1].refinement.?;
    const ptr_gid = refinements.at(eu_gid).errorunion.to;
    const pointee_gid = refinements.at(ptr_gid).pointer.to;
    const ms = refinements.at(pointee_gid).scalar.analyte.memory_safety.?;
    try std.testing.expect(ms == .allocated);
}

test "call intercepts mem.Allocator.destroy and marks allocation freed" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create allocator refinement
    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create pointee (scalar) with allocation metadata - destroy checks pointee's memory_safety
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = scalar_gid } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = alloc_gid; // allocator at inst 0
    results[1].refinement = ptr_gid; // pointer at inst 1

    const state = testState(&ctx, &results, &refinements);

    // Call mem.Allocator.destroy
    const args = &[_]tag.Src{ .{ .inst = 0 }, .{ .inst = 1 } }; // allocator, ptr
    try Inst.call(state, 2, null, .{ .void = {} }, args, "std.mem.Allocator.destroy");

    // Check the pointee is marked as freed
    const ms = refinements.at(scalar_gid).scalar.analyte.memory_safety.?;
    try std.testing.expect(ms == .allocated);
    try std.testing.expect(ms.allocated.freed != null);
}

test "destroy of interned pointer reports global memory free" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{
        .type_id = 100,
        .analyte = .{ .memory_safety = .{ .interned = ctx.meta } },
    } });
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .placeholder = {} } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = scalar_gid,
        .analyte = .{ .memory_safety = .{ .interned = ctx.meta } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = alloc_gid;
    results[1].refinement = ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    try std.testing.expectError(error.FreeGlobalMemory, Inst.call(state, 2, null, .{ .void = {} }, &.{
        .{ .inst = 0 },
        .{ .inst = 1 },
    }, "std.mem.Allocator.destroy"));
}

test "ret_load of struct with slice pointer field leaves no invalid orphaned memory safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    try ctx.push_fn("test_func");
    defer ctx.pop_fn();

    var results = [_]Inst{.{}} ** 9;
    var early_returns = std.ArrayListUnmanaged(State){};
    defer Inst.freeEarlyReturns(&early_returns, ctx.allocator);
    const caller_meta = core.Meta{ .function = "caller", .file = "caller.zig", .line = 1, .column = 1 };

    const arg_elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = caller_meta, .root_gid = null } } },
    } });
    const arg_region_gid = try refinements.appendEntity(.{ .region = .{
        .to = arg_elem_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = caller_meta, .root_gid = null } } },
    } });
    const arg_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = arg_region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = caller_meta, .root_gid = null } } },
    } });

    const struct_ty: tag.Type = .{ .@"struct" = &.{
        .type_id = 2706,
        .fields = &.{
            .{ .scalar = {} },
            .{ .pointer = &.{ .region = &.{ .scalar = {} } } },
        },
    } };
    const return_ref = try tag.typeToRefinement(struct_ty, &refinements);
    const return_gid = try refinements.appendEntity(return_ref);
    tag.splatInitCallReturnSlot(&refinements, return_gid, &ctx);

    const state = State{
        .ctx = &ctx,
        .results = &results,
        .refinements = &refinements,
        .return_gid = return_gid,
        .base_gid = return_gid,
        .early_returns = &early_returns,
        .restrict = .memory_safety,
    };

    try Inst.apply(state, 0, .{ .arg = .{ .value = arg_ptr_gid, .name_id = 1 } });
    try Inst.apply(state, 1, .{ .ret_ptr = .{ .ty = struct_ty } });
    try Inst.apply(state, 2, .{ .struct_field_ptr = .{
        .base = .{ .inst = 1 },
        .field_index = 1,
        .ty = .{ .pointer = &.{ .pointer = &.{ .region = &.{ .scalar = {} } } } },
        .type_id = 0,
    } });
    try Inst.apply(state, 3, .{ .store = .{ .ptr = .{ .inst = 2 }, .src = .{ .inst = 0 } } });
    try Inst.apply(state, 4, .{ .struct_field_ptr = .{
        .base = .{ .inst = 1 },
        .field_index = 0,
        .ty = .{ .pointer = &.{ .scalar = {} } },
        .type_id = 0,
    } });
    try Inst.apply(state, 5, .{ .store = .{ .ptr = .{ .inst = 4 }, .src = .{ .interned = .{ .ip_idx = 109, .ty = .{ .scalar = {} } } } } });
    try Inst.apply(state, 6, .{ .ret_load = .{ .ptr = 1 } });
    try Inst.mergeEarlyReturns(state);

    for (refinements.list.items[return_gid..], return_gid..) |refinement, idx| {
        @import("memory_safety.zig").testValid(refinement, idx);
    }
}

test "allocation returned through block break is not reported as callee leak" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    try ctx.push_fn("allocate_with");
    defer ctx.pop_fn();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{
        .type_id = 100,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const return_ref = try tag.typeToRefinement(.{ .pointer = &.{ .scalar = {} } }, &refinements);
    const return_gid = try refinements.appendEntity(return_ref);
    tag.splatInitCallReturnSlot(&refinements, return_gid, &ctx);

    var results = [_]Inst{.{}} ** 8;
    results[0].refinement = alloc_gid;
    var early_returns = std.ArrayListUnmanaged(State){};
    defer Inst.freeEarlyReturns(&early_returns, ctx.allocator);
    const state = State{
        .ctx = &ctx,
        .results = &results,
        .refinements = &refinements,
        .return_gid = return_gid,
        .base_gid = return_gid,
        .early_returns = &early_returns,
        .restrict = .memory_safety,
    };

    try Inst.call(state, 1, null, .{ .errorunion = &.{ .pointer = &.{ .scalar = {} } } }, &.{.{ .inst = 0 }}, "std.mem.Allocator.create");
    try Inst.apply(state, 2, .{ .block = .{ .ty = .{ .pointer = &.{ .scalar = {} } } } });
    try Inst.apply(state, 3, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 1 } } });
    try Inst.apply(state, 4, .{ .br = .{ .block = 2, .src = .{ .inst = 3 } } });
    try Inst.apply(state, 5, .{ .ret_safe = .{ .src = .{ .inst = 2 } } });
    try Inst.mergeEarlyReturns(state);
    try Inst.onFinish(state);
}

test "reachable allocations are tracked by allocation root gid, not allocator gid" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{
        .type_id = 100,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const scalar_a = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const scalar_b = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const ptr_a = try refinements.appendEntity(.{ .pointer = .{ .to = scalar_a } });
    const ptr_b = try refinements.appendEntity(.{ .pointer = .{ .to = scalar_b } });
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = try ctx.allocator.dupe(Gid, &.{ ptr_a, ptr_b }),
        .type_id = 200,
    } });

    var roots = std.AutoHashMap(Gid, void).init(ctx.allocator);
    defer roots.deinit();
    MemorySafety.collectReachableAllocationsForTest(&refinements, struct_gid, &roots);

    try std.testing.expect(roots.contains(scalar_a));
    try std.testing.expect(roots.contains(scalar_b));
    try std.testing.expect(!roots.contains(alloc_gid));
}

test "slice from ptr_add is treated as derived pointer on free" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const base_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 5;
    results[0].refinement = alloc_gid;
    results[1].refinement = base_ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    try Inst.apply(state, 2, .{ .ptr_add = .{ .ptr = .{ .inst = 1 } } });
    try Inst.apply(state, 3, .{ .slice = .{
        .ptr = .{ .inst = 2 },
        .ty = .{ .pointer = &.{ .region = &.{ .scalar = {} } } },
    } });

    try std.testing.expectError(error.FreeFieldPointer, Inst.call(state, 4, null, .{ .void = {} }, &.{
        .{ .inst = 0 },
        .{ .inst = 3 },
    }, "std.mem.Allocator.free"));
}

test "slice from zero ptr_add preserves base pointer provenance on free" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const base_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 5;
    results[0].refinement = alloc_gid;
    results[1].refinement = base_ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    try Inst.apply(state, 2, .{ .ptr_add = .{ .ptr = .{ .inst = 1 }, .offset_is_zero = true } });
    try Inst.apply(state, 3, .{ .slice = .{
        .ty = .{ .pointer = &.{ .region = &.{ .scalar = {} } } },
        .ptr = .{ .inst = 2 },
    } });

    try Inst.call(state, 4, null, .{ .void = {} }, &.{
        .{ .inst = 0 },
        .{ .inst = 3 },
    }, "std.mem.Allocator.free");
}

test "ptr_sub from derived pointer remains derived on free" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const base_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 6;
    results[0].refinement = alloc_gid;
    results[1].refinement = base_ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    try Inst.apply(state, 2, .{ .ptr_add = .{ .ptr = .{ .inst = 1 } } });
    try Inst.apply(state, 3, .{ .ptr_sub = .{ .ptr = .{ .inst = 2 } } });
    try Inst.apply(state, 4, .{ .slice = .{
        .ptr = .{ .inst = 3 },
        .ty = .{ .pointer = &.{ .region = &.{ .scalar = {} } } },
    } });

    try std.testing.expectError(error.FreeFieldPointer, Inst.call(state, 5, null, .{ .void = {} }, &.{
        .{ .inst = 0 },
        .{ .inst = 4 },
    }, "std.mem.Allocator.free"));
}

test "ptr_sub derived provenance survives pointer slot store load before free" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const base_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 12;
    results[0].refinement = alloc_gid;
    results[1].refinement = base_ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    try Inst.apply(state, 2, .{ .ptr_add = .{ .ptr = .{ .inst = 1 } } });
    try Inst.apply(state, 3, .{ .ptr_sub = .{ .ptr = .{ .inst = 2 } } });
    try Inst.apply(state, 4, .{ .alloc = .{ .ty = .{ .pointer = &.{ .region = &.{ .scalar = {} } } } } });
    try Inst.apply(state, 5, .{ .store = .{ .ptr = .{ .inst = 4 }, .src = .{ .inst = 3 } } });
    try Inst.apply(state, 6, .{ .bitcast = .{
        .src = .{ .inst = 4 },
        .ty = .{ .pointer = &.{ .pointer = &.{ .region = &.{ .scalar = {} } } } },
    } });
    try Inst.apply(state, 7, .{ .load = .{ .ptr = .{ .inst = 6 } } });
    try Inst.apply(state, 8, .{ .ptr_add = .{ .ptr = .{ .inst = 7 }, .offset_is_zero = true } });
    try Inst.apply(state, 9, .{ .bitcast = .{
        .src = .{ .inst = 8 },
        .ty = .{ .pointer = &.{ .region = &.{ .scalar = {} } } },
    } });
    try Inst.apply(state, 10, .{ .slice = .{
        .ptr = .{ .inst = 9 },
        .ty = .{ .pointer = &.{ .region = &.{ .scalar = {} } } },
    } });

    try std.testing.expectError(error.FreeFieldPointer, Inst.call(state, 11, null, .{ .void = {} }, &.{
        .{ .inst = 0 },
        .{ .inst = 10 },
    }, "std.mem.Allocator.free"));
}

test "bitcast preserves derived pointer provenance on free" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const base_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 6;
    results[0].refinement = alloc_gid;
    results[1].refinement = base_ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    try Inst.apply(state, 2, .{ .ptr_add = .{ .ptr = .{ .inst = 1 } } });
    try Inst.apply(state, 3, .{ .bitcast = .{
        .src = .{ .inst = 2 },
        .ty = .{ .pointer = &.{ .region = &.{ .scalar = {} } } },
    } });

    try std.testing.expectError(error.FreeFieldPointer, Inst.call(state, 4, null, .{ .void = {} }, &.{
        .{ .inst = 0 },
        .{ .inst = 3 },
    }, "std.mem.Allocator.free"));
}

test "bitcast to optional pointer initializes copied pointer target memory safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = 0,
            .type_id = 100,
        } } },
    } });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = 0,
            .type_id = 100,
        } } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    const struct_type = core.Type{ .@"struct" = &.{
        .type_id = 1234,
        .fields = &.{ .{ .scalar = {} }, .{ .scalar = {} } },
        .is_packed = true,
    } };
    try Inst.apply(state, 1, .{ .bitcast = .{
        .src = .{ .inst = 0 },
        .ty = .{ .optional = &.{ .pointer = &.{ .region = &struct_type } } },
    } });

    const opt_ref = refinements.at(results[1].refinement.?);
    try std.testing.expectEqual(.optional, std.meta.activeTag(opt_ref.*));
    try expectMemorySafetySet(refinements.at(results[1].refinement.?));
    const payload_ref = refinements.at(opt_ref.optional.to);
    try std.testing.expectEqual(.pointer, std.meta.activeTag(payload_ref.*));
    try expectMemorySafetySet(payload_ref);
    try expectMemorySafetySet(refinements.at(payload_ref.pointer.to));
}

test "branch orphan ignores allocation reachable through merge-created parent value" {
    var ctx, var parent = initTest();
    defer ctx.deinit();
    defer parent.deinit();

    const alloc_gid = try parent.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const base_len: Gid = @intCast(parent.list.items.len);
    const elem_gid = try parent.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const region_gid = try parent.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const reachable_ptr_gid = try parent.appendEntity(.{ .pointer = .{ .to = region_gid } });

    var branch = try parent.clone(ctx.allocator);
    defer branch.deinit();

    var copied = std.AutoHashMap(Gid, void).init(ctx.allocator);
    defer copied.deinit();

    try tag.splatOrphaned(&ctx, &parent, &branch, reachable_ptr_gid, &copied, .{ .branch_merge = .{
        .meta = ctx.meta,
        .branch_type = .cond_br,
        .base_len = base_len,
    } });
}

test "branch orphan matches imported allocation with different parent gid" {
    var ctx, var parent = initTest();
    defer ctx.deinit();
    defer parent.deinit();

    const alloc_gid = try parent.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const base_len: Gid = @intCast(parent.list.items.len);

    var branch = try parent.clone(ctx.allocator);
    defer branch.deinit();

    const branch_elem_gid = try branch.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const branch_region_gid = try branch.appendEntity(.{ .region = .{
        .to = branch_elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const branch_ptr_gid = try branch.appendEntity(.{ .pointer = .{ .to = branch_region_gid } });

    _ = try parent.appendEntity(.{ .void = {} });
    _ = try parent.appendEntity(.{ .void = {} });
    const parent_elem_gid = try parent.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const parent_region_gid = try parent.appendEntity(.{ .region = .{
        .to = parent_elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    _ = try parent.appendEntity(.{ .pointer = .{ .to = parent_region_gid } });

    var copied = std.AutoHashMap(Gid, void).init(ctx.allocator);
    defer copied.deinit();

    try tag.splatOrphaned(&ctx, &parent, &branch, branch_ptr_gid, &copied, .{ .branch_merge = .{
        .meta = ctx.meta,
        .branch_type = .cond_br,
        .base_len = base_len,
    } });
}

test "branch orphan matches allocation reachable through copied argument" {
    var ctx, var parent = initTest();
    defer ctx.deinit();
    defer parent.deinit();

    const alloc_gid = try parent.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    var branch = try parent.clone(ctx.allocator);
    defer branch.deinit();

    const arg_elem_gid = try branch.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const arg_ptr_gid = try branch.appendEntity(.{ .pointer = .{
        .to = arg_elem_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const arg_struct_gid = try branch.appendEntity(.{ .@"struct" = .{
        .fields = try ctx.allocator.dupe(Gid, &.{arg_ptr_gid}),
        .type_id = 200,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    _ = arg_struct_gid;
    const base_len: Gid = @intCast(branch.list.items.len);

    const orphan_elem_gid = try branch.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const orphan_ptr_gid = try branch.appendEntity(.{ .pointer = .{
        .to = orphan_elem_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var copied = std.AutoHashMap(Gid, void).init(ctx.allocator);
    defer copied.deinit();

    try tag.splatOrphaned(&ctx, &parent, &branch, orphan_ptr_gid, &copied, .{ .branch_merge = .{
        .meta = ctx.meta,
        .branch_type = .cond_br,
        .base_len = base_len,
    } });
}

test "early return ignores allocation reachable through copied argument" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    try ctx.push_fn("test_func");
    defer ctx.pop_fn();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const arg_elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const arg_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = arg_elem_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const arg_struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = try ctx.allocator.dupe(Gid, &.{arg_ptr_gid}),
        .type_id = 200,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const base_gid: Gid = @intCast(refinements.list.items.len);

    const local_elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const local_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = local_elem_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = arg_struct_gid;
    results[1].refinement = local_ptr_gid;
    const state = State{
        .ctx = &ctx,
        .results = &results,
        .refinements = &refinements,
        .return_gid = 0,
        .base_gid = base_gid,
        .restrict = .memory_safety,
    };

    try Inst.apply(state, 0, .{ .arg = .{ .value = arg_struct_gid, .name_id = 1 } });
    try Inst.apply(state, 0, .{ .cond_br = .{ .branch = true, .condition_idx = null } });
    try Inst.apply(state, 2, .{ .ret_safe = .{ .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = {} } } } } });
}

test "divergent allocated pointer branch result does not orphan alternate allocation" {
    var ctx, var parent = initTest();
    defer ctx.deinit();
    defer parent.deinit();

    const alloc_gid = try parent.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const placeholder_elem = try parent.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .placeholder = {} } },
    } });
    const placeholder_region = try parent.appendEntity(.{ .region = .{
        .to = placeholder_elem,
        .analyte = .{ .memory_safety = .{ .placeholder = {} } },
    } });
    const parent_ptr = try parent.appendEntity(.{ .pointer = .{
        .to = placeholder_region,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const base_len: Gid = @intCast(parent.list.items.len);

    var parent_results = [_]Inst{.{ .refinement = parent_ptr }};
    var branch_results_a = [_]Inst{.{ .refinement = null }};
    var branch_results_b = [_]Inst{.{ .refinement = null }};

    var branch_a = try parent.clone(ctx.allocator);
    defer branch_a.deinit();
    var branch_b = try parent.clone(ctx.allocator);
    defer branch_b.deinit();

    const ptr_a = try appendAllocatedSlice(&ctx, &branch_a, alloc_gid, 10);
    const ptr_b = try appendAllocatedSlice(&ctx, &branch_b, alloc_gid, 20);
    branch_results_a[0].refinement = ptr_a;
    branch_results_b[0].refinement = ptr_b;

    var returns_a = false;
    var returns_b = false;
    const branches = [_]State{
        .{ .ctx = &ctx, .results = &branch_results_a, .refinements = &branch_a, .return_gid = 0, .branch_returns = &returns_a },
        .{ .ctx = &ctx, .results = &branch_results_b, .refinements = &branch_b, .return_gid = 0, .branch_returns = &returns_b },
    };

    try tag.splatMerge(.cond_br, &parent_results, &ctx, &parent, &branches, null, base_len, null);
}

fn appendAllocatedSlice(ctx: *Context, refinements: *Refinements, alloc_gid: Gid, type_id: u32) !Gid {
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = type_id,
        } } },
    } });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = type_id,
        } } },
    } });
    return refinements.appendEntity(.{ .pointer = .{ .to = region_gid } });
}

fn expectMemorySafetySet(ref: *const Refinements.Refinement) !void {
    const analyte = switch (ref.*) {
        .scalar => |s| s.analyte,
        .pointer => |p| p.analyte,
        .optional => |o| o.analyte,
        .errorunion => |e| e.analyte,
        .region => |r| r.analyte,
        .recursive => |r| r.analyte,
        .@"struct" => |s| s.analyte,
        .@"union" => |u| u.analyte,
        .fnptr => |f| f.analyte,
        .allocator => |a| a.analyte,
        .void, .noreturn, .unimplemented => return,
    };
    try std.testing.expect(analyte.memory_safety != null);
}

test "load detects use after free" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create allocator refinement
    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create pointee with allocation metadata (simulates created allocation)
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = scalar_gid } });

    var results = [_]Inst{.{}} ** 4;
    results[0].refinement = alloc_gid;
    results[1].refinement = ptr_gid;

    const state = testState(&ctx, &results, &refinements);

    // Destroy the allocation via call
    const destroy_args = &[_]tag.Src{ .{ .inst = 0 }, .{ .inst = 1 } };
    try Inst.call(state, 2, null, .{ .void = {} }, destroy_args, "std.mem.Allocator.destroy");

    // Load from freed pointer should error
    const result = Inst.apply(state, 3, .{ .load = .{ .ptr = .{ .inst = 1 } } });
    try std.testing.expectError(error.UseAfterFree, result);
}

test "memcpy detects use after free on dest" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create freed region (dest) - memory_safety on the region (slice allocation)
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = try refinements.appendEntity(.{
        .region = .{
            .to = elem_gid,
            .analyte = .{
                .memory_safety = .{
                    .allocated = .{
                        .meta = ctx.meta,
                        .root_gid = null,
                        .allocator_gid = alloc_gid,
                        .type_id = 100,
                        .freed = .{ .meta = ctx.meta }, // Already freed
                    },
                },
            },
        },
    });
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = region_gid } });

    // Create valid source region
    const src_elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const src_region_gid = try refinements.appendEntity(.{ .region = .{ .to = src_elem_gid } });
    const src_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = src_region_gid } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = dest_ptr_gid;
    results[1].refinement = src_ptr_gid;

    const state = testState(&ctx, &results, &refinements);

    // memcpy to freed dest should error
    const result = Inst.apply(state, 2, .{ .memcpy = .{ .dest = .{ .inst = 0 }, .src = .{ .inst = 1 } } });
    try std.testing.expectError(error.UseAfterFree, result);
}

test "memcpy detects use after free on src" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create valid dest region
    const dest_elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const dest_region_gid = try refinements.appendEntity(.{ .region = .{ .to = dest_elem_gid } });
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = dest_region_gid } });

    // Create freed source region - memory_safety on the region
    const src_elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const src_region_gid = try refinements.appendEntity(.{
        .region = .{
            .to = src_elem_gid,
            .analyte = .{
                .memory_safety = .{
                    .allocated = .{
                        .meta = ctx.meta,
                        .root_gid = null,
                        .allocator_gid = alloc_gid,
                        .type_id = 100,
                        .freed = .{ .meta = ctx.meta }, // Already freed
                    },
                },
            },
        },
    });
    const src_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = src_region_gid } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = dest_ptr_gid;
    results[1].refinement = src_ptr_gid;

    const state = testState(&ctx, &results, &refinements);

    // memcpy from freed src should error
    const result = Inst.apply(state, 2, .{ .memcpy = .{ .dest = .{ .inst = 0 }, .src = .{ .inst = 1 } } });
    try std.testing.expectError(error.UseAfterFree, result);
}

test "memmove detects use after free on dest" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create freed region (dest) - memory_safety on the region
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
            .freed = .{ .meta = ctx.meta },
        } } },
    } });
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = region_gid } });

    // Create valid source
    const src_elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const src_region_gid = try refinements.appendEntity(.{ .region = .{ .to = src_elem_gid } });
    const src_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = src_region_gid } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = dest_ptr_gid;
    results[1].refinement = src_ptr_gid;

    const state = testState(&ctx, &results, &refinements);

    const result = Inst.apply(state, 2, .{ .memmove = .{ .dest = .{ .inst = 0 }, .src = .{ .inst = 1 } } });
    try std.testing.expectError(error.UseAfterFree, result);
}

test "memset detects use after free on dest" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create freed region (dest) - memory_safety on the region
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
            .freed = .{ .meta = ctx.meta },
        } } },
    } });
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = region_gid } });

    // Value is just a scalar (the byte value to set)
    const value_gid = try refinements.appendEntity(.{ .scalar = .{} });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = dest_ptr_gid;
    results[1].refinement = value_gid;

    const state = testState(&ctx, &results, &refinements);

    const result = Inst.apply(state, 2, .{ .memset = .{ .dest = .{ .inst = 0 }, .value = .{ .inst = 1 } } });
    try std.testing.expectError(error.UseAfterFree, result);
}

test "memset_safe detects use after free on dest" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create freed region (dest) - memory_safety on the region
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
            .freed = .{ .meta = ctx.meta },
        } } },
    } });
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = region_gid } });

    const value_gid = try refinements.appendEntity(.{ .scalar = .{} });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = dest_ptr_gid;
    results[1].refinement = value_gid;

    const state = testState(&ctx, &results, &refinements);

    const result = Inst.apply(state, 2, .{ .memset_safe = .{ .dest = .{ .inst = 0 }, .value = .{ .inst = 1 } } });
    try std.testing.expectError(error.UseAfterFree, result);
}

test "memcpy succeeds with valid pointers" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create valid dest and src regions (stack allocations)
    const dest_elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const dest_region_gid = try refinements.appendEntity(.{ .region = .{ .to = dest_elem_gid } });
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = dest_region_gid } });

    const src_elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const src_region_gid = try refinements.appendEntity(.{ .region = .{ .to = src_elem_gid } });
    const src_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = src_region_gid } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = dest_ptr_gid;
    results[1].refinement = src_ptr_gid;

    const state = testState(&ctx, &results, &refinements);

    // Should succeed
    try Inst.apply(state, 2, .{ .memcpy = .{ .dest = .{ .inst = 0 }, .src = .{ .inst = 1 } } });
}

test "free on region with null memory_safety does not error" {
    // When a function returns a slice, the region may not have memory_safety tracking.
    // Freeing such a slice should not report "free of global/comptime memory".
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create allocator refinement
    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create a region with NO memory_safety (null) - simulates returned slice
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = try refinements.appendEntity(.{
        .region = .{
            .to = elem_gid,
            // analyte.memory_safety is null by default
        },
    });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = region_gid } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = alloc_gid;
    results[1].refinement = ptr_gid;

    const state = testState(&ctx, &results, &refinements);

    // Free the slice - should NOT error with FreeGlobalMemory
    const free_args = &[_]tag.Src{ .{ .inst = 0 }, .{ .inst = 1 } };
    // Currently this fails with FreeGlobalMemory, but it should succeed
    try Inst.call(state, 2, null, .{ .void = {} }, free_args, "std.mem.Allocator.free");
}

test "ret_safe does not report stack escape when pointer points to allocated memory" {
    // Regression test: When loading a slice/pointer from a struct field, the loaded
    // pointer VALUE gets memory_safety=.stack (it's a copy on the stack).
    // But the POINTEE (region) still has .allocated memory_safety.
    // ret_safe should check the POINTEE's memory_safety, not the pointer's.
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Use refinements' allocator for struct fields to avoid double-free
    const allocator = refinements.list.allocator;

    // Create allocator refinement for type tracking
    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create a region with .allocated memory_safety (simulating heap allocation)
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });

    // Create a pointer with .stack memory_safety (simulating load from struct field)
    // The pointer VALUE is on the stack, but it POINTS TO allocated memory
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{
            .meta = ctx.meta,
            .root_gid = null,
        } } },
    } });

    // Create a struct containing this pointer (like Container { .items = ... })
    const fields = try allocator.dupe(Gid, &.{ptr_gid});
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = fields,
        .type_id = 200,
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = struct_gid;

    ctx.stacktrace.append(allocator, "test_func") catch unreachable;
    const state = testState(&ctx, &results, &refinements);

    // ret_safe should NOT report a stack escape - the pointee is .allocated, not .stack
    try Inst.apply(state, 1, .{ .ret_safe = .{ .src = .{ .inst = 0 } } });

    // Test passes if we get here without error
    // refinements.deinit() handles cleanup of all fields
}

test "init with runtime leaves memory_safety null" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create a scalar refinement with null analytes
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{} });

    // Call init with runtime - should NOT set memory_safety
    tag.splatInit(&refinements, scalar_gid, &ctx, .runtime);

    // Verify memory_safety is still null
    const ms = refinements.at(scalar_gid).scalar.analyte.memory_safety;
    try std.testing.expect(ms == null);
}

test "init with defined sets memory_safety to interned" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create a scalar refinement with null analytes
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{} });

    // Call init with defined - should set memory_safety to interned
    tag.splatInit(&refinements, scalar_gid, &ctx, .defined);

    // Verify memory_safety is interned
    const ms = refinements.at(scalar_gid).scalar.analyte.memory_safety.?;
    try std.testing.expect(ms == .interned);
}

test "init with undefined sets memory_safety to interned" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create a scalar refinement with null analytes
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{} });

    // Call init with undefined - should set memory_safety to interned
    tag.splatInit(&refinements, scalar_gid, &ctx, .undefined);

    // Verify memory_safety is interned
    const ms = refinements.at(scalar_gid).scalar.analyte.memory_safety.?;
    try std.testing.expect(ms == .interned);
}

test "block initializes memory_safety for pointer types" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    const nested_type = tag.Type{ .pointer = &.{ .region = &.{ .scalar = {} } } };
    try Inst.apply(state, 0, .{ .block = .{ .ty = nested_type } });

    const ptr_gid = results[0].refinement.?;
    const ptr_ref = refinements.at(ptr_gid).*;
    try std.testing.expectEqual(.pointer, std.meta.activeTag(ptr_ref));

    const ptr_ms = ptr_ref.pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(ptr_ms));

    const region_gid = ptr_ref.pointer.to;
    const region_ref = refinements.at(region_gid).*;
    try std.testing.expectEqual(.region, std.meta.activeTag(region_ref));
    const region_ms = region_ref.region.analyte.memory_safety.?;
    try std.testing.expectEqual(.placeholder, std.meta.activeTag(region_ms));

    const scalar_gid = region_ref.region.to;
    const scalar_ref = refinements.at(scalar_gid).*;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(scalar_ref));
    const scalar_ms = scalar_ref.scalar.analyte.memory_safety.?;
    try std.testing.expectEqual(.placeholder, std.meta.activeTag(scalar_ms));
}

test "splatMerge propagates optional wrapper memory_safety from branches" {
    const allocator = std.testing.allocator;
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    ctx.meta.function = "merge_test";
    ctx.meta.file = "merge_test.zig";
    ctx.meta.line = 10;

    const payload_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = payload_gid } });

    var results = [_]Inst{.{}} ** 1;
    results[0].refinement = opt_gid;

    var branch1_refinements = try refinements.clone(allocator);
    defer branch1_refinements.deinit();
    var branch2_refinements = try refinements.clone(allocator);
    defer branch2_refinements.deinit();

    branch1_refinements.at(opt_gid).optional.analyte.memory_safety = .{ .stack = .{
        .meta = ctx.meta,
        .root_gid = null,
    } };
    branch2_refinements.at(opt_gid).optional.analyte.memory_safety = .{ .stack = .{
        .meta = ctx.meta,
        .root_gid = null,
    } };

    var branch1_results = results;
    var branch2_results = results;
    const branches = [_]State{
        testState(&ctx, &branch1_results, &branch1_refinements),
        testState(&ctx, &branch2_results, &branch2_refinements),
    };

    try tag.splatMerge(.cond_br, &results, &ctx, &refinements, &branches, null, null, null);

    try std.testing.expect(refinements.at(opt_gid).optional.analyte.memory_safety != null);
    try std.testing.expect(refinements.at(opt_gid).optional.analyte.memory_safety.? == .stack);
}

test "splatMerge propagates region wrapper memory_safety from branches" {
    const allocator = std.testing.allocator;
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    ctx.meta.function = "merge_test";
    ctx.meta.file = "merge_test.zig";
    ctx.meta.line = 20;

    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = try refinements.appendEntity(.{ .region = .{ .to = elem_gid } });

    var results = [_]Inst{.{}} ** 1;
    results[0].refinement = region_gid;

    var branch1_refinements = try refinements.clone(allocator);
    defer branch1_refinements.deinit();
    var branch2_refinements = try refinements.clone(allocator);
    defer branch2_refinements.deinit();

    branch1_refinements.at(region_gid).region.analyte.memory_safety = .{ .interned = ctx.meta };
    branch2_refinements.at(region_gid).region.analyte.memory_safety = .{ .interned = ctx.meta };

    var branch1_results = results;
    var branch2_results = results;
    const branches = [_]State{
        testState(&ctx, &branch1_results, &branch1_refinements),
        testState(&ctx, &branch2_results, &branch2_refinements),
    };

    try tag.splatMerge(.cond_br, &results, &ctx, &refinements, &branches, null, null, null);

    try std.testing.expect(refinements.at(region_gid).region.analyte.memory_safety != null);
    try std.testing.expect(refinements.at(region_gid).region.analyte.memory_safety.? == .interned);
}

test "memcpy propagates region element memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const dest_elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const dest_region_gid = try refinements.appendEntity(.{ .region = .{
        .to = dest_elem_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = dest_region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    const src_elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .interned = ctx.meta } },
    } });
    const src_region_gid = try refinements.appendEntity(.{ .region = .{
        .to = src_elem_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const src_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = src_region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = dest_ptr_gid;
    results[1].refinement = src_ptr_gid;

    const state = testState(&ctx, &results, &refinements);
    try Inst.apply(state, 2, .{ .memcpy = .{ .dest = .{ .inst = 0 }, .src = .{ .inst = 1 } } });

    try std.testing.expectEqual(.interned, std.meta.activeTag(refinements.at(dest_elem_gid).scalar.analyte.memory_safety.?));
}

test "boolean ops set result memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);
    const dummy_src = tag.Src{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = {} } } };

    try Inst.apply(state, 0, .{ .bool_or = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 1, .{ .bool_and = .{ .lhs = dummy_src, .rhs = dummy_src } });

    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(results[0].refinement.?).scalar.analyte.memory_safety.?));
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(results[1].refinement.?).scalar.analyte.memory_safety.?));
}

test "scalar arithmetic and cast ops set result memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 6;
    const state = testState(&ctx, &results, &refinements);
    const dummy_src = tag.Src{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = {} } } };

    try Inst.apply(state, 0, .{ .shr = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 1, .{ .shl = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 2, .{ .mul = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 3, .{ .min = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 4, .{ .not = .{ .src = dummy_src } });
    try Inst.apply(state, 5, .{ .trunc = .{ .src = dummy_src } });

    for (0..6) |i| {
        try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(results[i].refinement.?).scalar.analyte.memory_safety.?));
    }
}

test "select and reduce set result memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);
    const dummy_src = tag.Src{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = {} } } };

    try Inst.apply(state, 0, .{ .select = .{ .mask = dummy_src, .a = dummy_src, .b = dummy_src } });
    try Inst.apply(state, 1, .{ .reduce = .{ .src = .{ .inst = 0 } } });

    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(results[0].refinement.?).scalar.analyte.memory_safety.?));
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(results[1].refinement.?).scalar.analyte.memory_safety.?));
}

test "store interned pointer initializes destination pointer target memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = try refinements.appendEntity(.{ .region = .{ .to = elem_gid } });
    const pointer_slot_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const slot_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = pointer_slot_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = slot_ptr_gid;

    const state = testState(&ctx, &results, &refinements);
    try Inst.apply(state, 1, .{ .store = .{
        .ptr = .{ .inst = 0 },
        .src = .{ .interned = .{ .ip_idx = 1, .ty = .{ .pointer = &.{ .region = &.{ .scalar = {} } } } } },
    } });

    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(pointer_slot_gid).pointer.analyte.memory_safety.?));
    try std.testing.expectEqual(.interned, std.meta.activeTag(refinements.at(region_gid).region.analyte.memory_safety.?));
    try std.testing.expectEqual(.interned, std.meta.activeTag(refinements.at(elem_gid).scalar.analyte.memory_safety.?));
}

test "store interned struct initializes destination fields memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = try ctx.allocator.dupe(Gid, &.{scalar_gid}),
        .type_id = 100,
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = struct_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = ptr_gid;

    const state = testState(&ctx, &results, &refinements);
    try Inst.apply(state, 1, .{ .store = .{
        .ptr = .{ .inst = 0 },
        .src = .{ .interned = .{ .ip_idx = 1, .ty = .{ .@"struct" = &.{ .type_id = 100, .fields = &.{.{ .scalar = {} }} } } } },
    } });

    try std.testing.expectEqual(.interned, std.meta.activeTag(refinements.at(struct_gid).@"struct".analyte.memory_safety.?));
    try std.testing.expectEqual(.interned, std.meta.activeTag(refinements.at(scalar_gid).scalar.analyte.memory_safety.?));
}

test "store pointer into pointer slot does not paint old target as allocated" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const allocator_gid = try refinements.appendEntity(.{ .allocator = .{
        .type_id = 100,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const old_elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const old_region_gid = try refinements.appendEntity(.{ .region = .{
        .to = old_elem_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const slot_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = old_region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const slot_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = slot_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    const new_elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = allocator_gid,
            .type_id = 100,
        } } },
    } });
    const new_region_gid = try refinements.appendEntity(.{ .region = .{
        .to = new_elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = allocator_gid,
            .type_id = 100,
        } } },
    } });
    const src_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = new_region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = slot_ptr_gid;
    results[1].refinement = src_ptr_gid;

    const state = testState(&ctx, &results, &refinements);
    try Inst.apply(state, 2, .{ .store = .{ .ptr = .{ .inst = 0 }, .src = .{ .inst = 1 } } });

    try std.testing.expectEqual(new_region_gid, refinements.at(slot_gid).pointer.to);
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(old_region_gid).region.analyte.memory_safety.?));
    try std.testing.expectEqual(.allocated, std.meta.activeTag(refinements.at(new_region_gid).region.analyte.memory_safety.?));
}

test "store pointer into pointer slot reports clobbered live allocation" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const allocator_gid = try refinements.appendEntity(.{ .allocator = .{
        .type_id = 100,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const old_scalar_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = allocator_gid,
            .type_id = 100,
        } } },
    } });
    const slot_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = old_scalar_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const slot_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = slot_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const new_scalar_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = allocator_gid,
            .type_id = 100,
        } } },
    } });
    const src_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = new_scalar_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = slot_ptr_gid;
    results[1].refinement = src_ptr_gid;

    const state = testState(&ctx, &results, &refinements);
    try std.testing.expectError(error.MemoryLeak, Inst.apply(state, 2, .{ .store = .{
        .ptr = .{ .inst = 0 },
        .src = .{ .inst = 1 },
    } }));
}

test "aggregate_init sets struct container memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);
    const dummy_src = tag.Src{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = {} } } };

    try Inst.apply(state, 0, .{ .aggregate_init = .{
        .ty = .{ .@"struct" = &.{ .type_id = 100, .fields = &.{.{ .scalar = {} }} } },
        .elements = &.{dummy_src},
    } });

    const struct_gid = results[0].refinement.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(struct_gid).@"struct".analyte.memory_safety.?));
}
