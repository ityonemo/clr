const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;
const tag = @import("../tag.zig");
const Gid = Refinements.Gid;

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
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
            .freed = .{ .meta = ctx.meta }, // Already freed
        } } },
    } });
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
    const src_region_gid = try refinements.appendEntity(.{ .region = .{
        .to = src_elem_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
            .freed = .{ .meta = ctx.meta }, // Already freed
        } } },
    } });
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
    const region_gid = try refinements.appendEntity(.{ .region = .{
        .to = elem_gid,
        // analyte.memory_safety is null by default
    } });
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
