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

fn makeRegion(refinements: *Refinements, gid: Gid) Gid {
    refinements.at(gid).setMultiplicity(.region);
    return gid;
}

fn shouldNotExecuteProcessArgsOverride(_: *Context, _: *Refinements, _: Gid, _: []const Gid) anyerror!Gid {
    return error.ProcessArgsOverrideDidNotIntercept;
}

test "alloc sets stack metadata on pointee" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Alloc creates a stack pointer
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = .{} } } });

    // Check pointer has stack metadata
    const ptr_gid = results[0].refinement.?;
    const ms = refinements.at(ptr_gid).pointer.analyte.memory_safety.?;
    try std.testing.expect(ms == .stack);
}

test "undefined region type initializes placeholder memory_safety recursively" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const ref = try tag.typeToRefinement(.{ .undefined = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } }, &refinements);
    const region_gid = try refinements.appendEntity(ref);
    const elem_gid = region_gid;

    try std.testing.expectEqual(.placeholder, std.meta.activeTag(refinements.at(region_gid).scalar.analyte.memory_safety.?));
    try std.testing.expectEqual(.placeholder, std.meta.activeTag(refinements.at(elem_gid).scalar.analyte.memory_safety.?));
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
            .{ .scalar = .{} },
            .{ .pointer = .{ .to = &.{ .@"union" = &.{
                .type_id = 100,
                .variants = &.{ .{ .scalar = .{} }, .{ .pointer = .{ .to = &.{ .recursive = 100 } } } },
            } } } },
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
        .ty = .{ .pointer = .{ .to = &union_type.@"union".variants[1] } },
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

test "hashmap_header inherits memory safety from metadata pointer" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const allocation: MemorySafety = .{ .allocated = .{ .meta = ctx.meta, .root_gid = null, .allocator_gid = 1, .type_id = 0 } };
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = allocation },
    } });
    const region_gid = makeRegion(&refinements, elem_gid);
    const metadata_payload_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = allocation },
    } });
    const metadata_gid = try refinements.appendEntity(.{ .optional = .{
        .to = metadata_payload_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const size_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const fields = try std.testing.allocator.alloc(Gid, 2);
    fields[0] = metadata_gid;
    fields[1] = size_gid;
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .type_id = 100,
        .fields = fields,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = struct_gid;
    const state = testState(&ctx, &results, &refinements);

    try Inst.apply(state, 1, .{ .hashmap_header = .{
        .self = .{ .inst = 0 },
        .ty = .{ .pointer = .{ .to = &.{ .@"struct" = &.{ .type_id = 101, .fields = &.{.{ .scalar = .{} }} } } } },
    } });

    const result_gid = results[1].refinement.?;
    try std.testing.expectEqual(.allocated, std.meta.activeTag(refinements.at(result_gid).pointer.analyte.memory_safety.?));
    const pointee_gid = refinements.at(result_gid).pointer.to;
    try std.testing.expectEqual(.allocated, std.meta.activeTag(refinements.at(pointee_gid).@"struct".analyte.memory_safety.?));
}

test "hashmap_header treats placeholder metadata as synthetic valid header" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .placeholder = {} } },
    } });
    const region_gid = makeRegion(&refinements, elem_gid);
    const metadata_payload_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .placeholder = {} } },
    } });
    const metadata_gid = try refinements.appendEntity(.{ .optional = .{ .to = metadata_payload_gid } });
    const size_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try std.testing.allocator.alloc(Gid, 2);
    fields[0] = metadata_gid;
    fields[1] = size_gid;
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .type_id = 100,
        .fields = fields,
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = struct_gid;
    const state = testState(&ctx, &results, &refinements);

    try Inst.apply(state, 1, .{ .hashmap_header = .{
        .self = .{ .inst = 0 },
        .ty = .{ .pointer = .{ .to = &.{ .@"struct" = &.{ .type_id = 101, .fields = &.{.{ .scalar = .{} }} } } } },
    } });

    const result_gid = results[1].refinement.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(result_gid).pointer.analyte.memory_safety.?));
    const pointee_gid = refinements.at(result_gid).pointer.to;
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(pointee_gid).@"struct".analyte.memory_safety.?));
}

test "array_elem_val from region value copies element memory safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const region_gid = makeRegion(&refinements, elem_gid);
    results[0].refinement = region_gid;

    try Inst.apply(state, 1, .{ .array_elem_val = .{ .base = .{ .inst = 0 } } });

    const result_gid = results[1].refinement.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(result_gid).scalar.analyte.memory_safety.?));
}

test "struct_field_ptr through reinterpreted region inherits base pointer memory" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .placeholder = {} } },
    } });
    const region_gid = makeRegion(&refinements, elem_gid);
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = region_gid } } },
    } });
    results[0].refinement = ptr_gid;

    try Inst.apply(state, 1, .{ .struct_field_ptr = .{
        .base = .{ .inst = 0 },
        .field_index = 0,
        .ty = .{ .pointer = .{ .to = &.{ .scalar = .{} } } },
        .type_id = 123,
    } });

    const field_ptr_gid = results[1].refinement.?;
    const field_ptr_ms = refinements.at(field_ptr_gid).pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(field_ptr_ms));

    const field_gid = refinements.at(field_ptr_gid).pointer.to;
    const field_ms = refinements.at(field_gid).scalar.analyte.memory_safety.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(field_ms));
}

test "struct_field_ptr through reinterpreted region initializes pointer field target" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    const elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .placeholder = {} } },
    } });
    const region_gid = makeRegion(&refinements, elem_gid);
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = region_gid } } },
    } });
    results[0].refinement = ptr_gid;

    try Inst.apply(state, 1, .{ .struct_field_ptr = .{
        .base = .{ .inst = 0 },
        .field_index = 0,
        .ty = .{ .pointer = .{ .to = &.{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } } } },
        .type_id = 123,
    } });

    const field_ptr_gid = results[1].refinement.?;
    const field_gid = refinements.at(field_ptr_gid).pointer.to;
    const nested_region_gid = refinements.at(field_gid).pointer.to;
    const nested_elem_gid = nested_region_gid;

    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(field_gid).pointer.analyte.memory_safety.?));
    try std.testing.expectEqual(.placeholder, std.meta.activeTag(refinements.at(nested_region_gid).scalar.analyte.memory_safety.?));
    try std.testing.expectEqual(.placeholder, std.meta.activeTag(refinements.at(nested_elem_gid).scalar.analyte.memory_safety.?));
}

test "bitcast initializes missing children under initialized pointer target" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    const field_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const target_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = try refinements.list.allocator.dupe(Gid, &.{field_gid}),
        .type_id = 123,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = target_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    results[0].refinement = ptr_gid;

    try Inst.apply(state, 1, .{ .bitcast = .{
        .src = .{ .inst = 0 },
        .ty = .{ .pointer = .{ .to = &.{ .@"struct" = &.{ .type_id = 123, .fields = &.{.{ .scalar = .{} }} } } } },
    } });

    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(field_gid).scalar.analyte.memory_safety.?));
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
    const scalar_type: tag.Type = .{ .scalar = .{} };
    const ptr_type: tag.Type = .{ .pointer = .{ .to = &scalar_type } };
    const return_type: tag.Type = .{ .errorunion = .{ .to = &ptr_type } };
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

test "call intercepts HashMap Metadata mutator" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    const return_gid = try refinements.appendEntity(.{ .void = {} });
    results[0].refinement = return_gid;

    const intercepted = try MemorySafety.call(
        state,
        0,
        .{ .void = {} },
        &.{},
        "hash_map.HashMapUnmanaged(u32,u32,hash_map.AutoContext(u32),80).Metadata.fill",
    );

    try std.testing.expect(intercepted);
}

test "call intercepts HashMap keys accessor with metadata-backed pointer view" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{
        .type_id = 100,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const metadata_region_gid = try refinements.appendEntity(.{ .scalar = .{
        .multiplicity = .region,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const metadata_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = metadata_region_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = metadata_region_gid,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const metadata_gid = try refinements.appendEntity(.{ .optional = .{ .to = metadata_ptr_gid } });
    const size_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try std.testing.allocator.alloc(Gid, 2);
    fields[0] = metadata_gid;
    fields[1] = size_gid;
    const self_gid = try refinements.appendEntity(.{ .@"struct" = .{ .type_id = 200, .fields = fields } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = self_gid;
    const state = testState(&ctx, &results, &refinements);

    const result_region_gid = try refinements.appendEntity(.{ .scalar = .{ .multiplicity = .region } });
    const result_gid = try refinements.appendEntity(.{ .pointer = .{ .to = result_region_gid } });
    results[1].refinement = result_gid;

    const intercepted = try MemorySafety.call(
        state,
        1,
        .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } },
        &.{.{ .inst = 0 }},
        "hash_map.HashMapUnmanaged(u32,u32,hash_map.AutoContext(u32),80).keys",
    );

    try std.testing.expect(intercepted);
    const result_ref = refinements.at(result_gid);
    try std.testing.expect(result_ref.pointer.analyte.memory_safety != null);
    const pointee_ms = refinements.at(result_ref.pointer.to).scalar.analyte.memory_safety.?;
    try std.testing.expect(pointee_ms != .placeholder);
}

test "call intercepts HashMap capacity-assume mutator" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    const return_gid = try refinements.appendEntity(.{ .void = {} });
    results[0].refinement = return_gid;

    const intercepted = try MemorySafety.call(
        state,
        0,
        .{ .void = {} },
        &.{},
        "hash_map.HashMapUnmanaged(u32,u32,hash_map.AutoContext(u32),80).putAssumeCapacityNoClobberContext",
    );

    try std.testing.expect(intercepted);
}

test "call intercepts public HashMap put" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    results[0].refinement = try refinements.appendEntity(.{ .void = {} });

    const intercepted = try MemorySafety.call(
        state,
        0,
        .{ .void = {} },
        &.{},
        "hash_map.HashMap(u32,u32,hash_map.AutoContext(u32),80).put",
    );

    try std.testing.expect(intercepted);
}

test "call intercepts process args iterator init" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    const return_type = tag.Type{ .@"struct" = &.{
        .type_id = 2709,
        .fields = &.{ .{ .scalar = .{} }, .{ .scalar = .{} } },
    } };

    try Inst.call(
        state,
        0,
        shouldNotExecuteProcessArgsOverride,
        return_type,
        &.{},
        "process.ArgIteratorPosix.init",
    );

    const result_gid = results[0].refinement.?;
    const result_ref = refinements.at(result_gid);
    try std.testing.expect(result_ref.* == .scalar);
    try std.testing.expectEqual(.interned, std.meta.activeTag(result_ref.scalar.analyte.memory_safety.?));
    try std.testing.expectEqual(.defined, std.meta.activeTag(result_ref.scalar.analyte.undefined_safety.?));
}

test "call intercepts process args iterator next" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const iterator_slot_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{
            .undefined_safety = .{ .defined = {} },
            .memory_safety = .{ .interned = ctx.meta },
        },
    } });
    const iterator_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = iterator_slot_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = iterator_ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    const return_type = tag.Type{ .optional = .{ .to = &.{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } } } };

    try Inst.call(
        state,
        1,
        shouldNotExecuteProcessArgsOverride,
        return_type,
        &.{.{ .inst = 0 }},
        "process.ArgIteratorPosix.next",
    );

    const result_gid = results[1].refinement.?;
    const result_ref = refinements.at(result_gid);
    try std.testing.expect(result_ref.* == .optional);
    try std.testing.expectEqual(.interned, std.meta.activeTag(result_ref.optional.analyte.memory_safety.?));

    const payload_ref = refinements.at(result_ref.optional.to);
    try std.testing.expect(payload_ref.* == .pointer);
    try std.testing.expectEqual(.interned, std.meta.activeTag(payload_ref.pointer.analyte.memory_safety.?));

    const region_ref = refinements.at(payload_ref.pointer.to);
    try std.testing.expect(region_ref.* == .scalar);
    try std.testing.expectEqual(.region, region_ref.scalar.multiplicity);
    try std.testing.expectEqual(.interned, std.meta.activeTag(region_ref.scalar.analyte.memory_safety.?));
}

test "call intercepts HashMap deallocate and frees metadata allocation" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{
        .type_id = 100,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const metadata_region_gid = try refinements.appendEntity(.{ .scalar = .{
        .multiplicity = .region,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const metadata_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = metadata_region_gid,
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = metadata_region_gid,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const metadata_gid = try refinements.appendEntity(.{ .optional = .{ .to = metadata_ptr_gid } });
    const size_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try std.testing.allocator.alloc(Gid, 2);
    fields[0] = metadata_gid;
    fields[1] = size_gid;
    const self_gid = try refinements.appendEntity(.{ .@"struct" = .{ .type_id = 200, .fields = fields } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = self_gid;
    results[1].refinement = try refinements.appendEntity(.{ .void = {} });
    const state = testState(&ctx, &results, &refinements);

    const intercepted = try MemorySafety.call(
        state,
        1,
        .{ .void = {} },
        &.{ .{ .inst = 0 }, .{ .inst = 0 } },
        "hash_map.HashMapUnmanaged(u32,u32,hash_map.AutoContext(u32),80).deallocate",
    );

    try std.testing.expect(intercepted);
    const ms = refinements.at(metadata_region_gid).scalar.analyte.memory_safety.?;
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

test "destroy of interned pointer value to allocated pointee frees allocation" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{
        .type_id = 100,
        .analyte = .{ .memory_safety = .{ .interned = ctx.meta } },
    } });
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = scalar_gid,
        .analyte = .{ .memory_safety = .{ .interned = ctx.meta } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = alloc_gid;
    results[1].refinement = ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    try Inst.call(state, 2, null, .{ .void = {} }, &.{
        .{ .inst = 0 },
        .{ .inst = 1 },
    }, "std.mem.Allocator.destroy");

    const ms = refinements.at(scalar_gid).scalar.analyte.memory_safety.?;
    try std.testing.expect(ms == .allocated);
    try std.testing.expect(ms.allocated.freed != null);
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
    const arg_region_gid = makeRegion(&refinements, arg_elem_gid);
    const arg_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = arg_region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = caller_meta, .root_gid = null } } },
    } });

    const struct_ty: tag.Type = .{ .@"struct" = &.{
        .type_id = 2706,
        .fields = &.{
            .{ .scalar = .{} },
            .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } },
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
        .ty = .{ .pointer = .{ .to = &.{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } } } },
        .type_id = 0,
    } });
    try Inst.apply(state, 3, .{ .store = .{ .ptr = .{ .inst = 2 }, .src = .{ .inst = 0 } } });
    try Inst.apply(state, 4, .{ .struct_field_ptr = .{
        .base = .{ .inst = 1 },
        .field_index = 0,
        .ty = .{ .pointer = .{ .to = &.{ .scalar = .{} } } },
        .type_id = 0,
    } });
    try Inst.apply(state, 5, .{ .store = .{ .ptr = .{ .inst = 4 }, .src = .{ .interned = .{ .ip_idx = 109, .ty = .{ .scalar = .{} } } } } });
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
    const return_ref = try tag.typeToRefinement(.{ .pointer = .{ .to = &.{ .scalar = .{} } } }, &refinements);
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

    try Inst.call(state, 1, null, .{ .errorunion = .{ .to = &.{ .pointer = .{ .to = &.{ .scalar = .{} } } } } }, &.{.{ .inst = 0 }}, "std.mem.Allocator.create");
    try Inst.apply(state, 2, .{ .block = .{ .ty = .{ .pointer = .{ .to = &.{ .scalar = .{} } } } } });
    try Inst.apply(state, 3, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 1 } } });
    try Inst.apply(state, 4, .{ .br = .{ .block = 2, .src = .{ .inst = 3 } } });
    try Inst.apply(state, 5, .{ .ret_safe = .{ .src = .{ .inst = 2 } } });
    try Inst.mergeEarlyReturns(state);
    try Inst.onFinish(state);
}

test "local pointer slot containing argument allocation is not reported as callee leak" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    try ctx.push_fn("stores_arg_pointer");
    defer ctx.pop_fn();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{
        .type_id = 100,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const allocated_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .allocated = .{
            .meta = ctx.meta,
            .root_gid = null,
            .allocator_gid = alloc_gid,
            .type_id = 100,
        } } },
    } });
    const arg_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = allocated_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const return_gid = try refinements.appendEntity(.{ .void = {} });

    var results = [_]Inst{.{}} ** 4;
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

    try Inst.apply(state, 0, .{ .arg = .{ .value = arg_ptr_gid, .name_id = 0 } });
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .pointer = .{ .to = &.{ .scalar = .{} } } } } });
    try Inst.apply(state, 2, .{ .store = .{ .ptr = .{ .inst = 1 }, .src = .{ .inst = 0 } } });
    try Inst.apply(state, 3, .{ .ret_safe = .{ .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .void = {} } } } } });
    try Inst.mergeEarlyReturns(state);

    try Inst.onFinish(state);
}

test "ret_safe ignores invalid root gid while collecting argument reachability" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const pointee_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = pointee_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = core.INVALID_GID } } },
    } });
    const return_gid = try refinements.appendEntity(.{ .void = {} });

    var results = [_]Inst{.{}} ** 2;
    const state = State{
        .ctx = &ctx,
        .results = &results,
        .refinements = &refinements,
        .return_gid = return_gid,
        .base_gid = return_gid,
        .restrict = .memory_safety,
    };

    try Inst.apply(state, 0, .{ .arg = .{ .value = ptr_gid, .name_id = 0 } });
    try Inst.apply(state, 1, .{ .ret_safe = .{ .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .void = {} } } } } });
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

test "reachable allocation collection skips pointer to no-analyte target" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    _ = &ctx;

    const void_gid = try refinements.appendEntity(.void);
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = void_gid,
    } });

    var allocs = std.AutoHashMap(Gid, void).init(std.testing.allocator);
    defer allocs.deinit();

    MemorySafety.collectReachableAllocationsForTest(&refinements, ptr_gid, &allocs);
    try std.testing.expectEqual(@as(usize, 0), allocs.count());
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
    const region_gid = makeRegion(&refinements, elem_gid);
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
        .ty = .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } },
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
    const region_gid = makeRegion(&refinements, elem_gid);
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
        .ty = .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } },
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
    const region_gid = makeRegion(&refinements, elem_gid);
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
        .ty = .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } },
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
    const region_gid = makeRegion(&refinements, elem_gid);
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
    try Inst.apply(state, 4, .{ .alloc = .{ .ty = .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } } } });
    try Inst.apply(state, 5, .{ .store = .{ .ptr = .{ .inst = 4 }, .src = .{ .inst = 3 } } });
    try Inst.apply(state, 6, .{ .bitcast = .{
        .src = .{ .inst = 4 },
        .ty = .{ .pointer = .{ .to = &.{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } } } },
    } });
    try Inst.apply(state, 7, .{ .load = .{ .ptr = .{ .inst = 6 } } });
    try Inst.apply(state, 8, .{ .ptr_add = .{ .ptr = .{ .inst = 7 }, .offset_is_zero = true } });
    try Inst.apply(state, 9, .{ .bitcast = .{
        .src = .{ .inst = 8 },
        .ty = .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } },
    } });
    try Inst.apply(state, 10, .{ .slice = .{
        .ptr = .{ .inst = 9 },
        .ty = .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } },
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
    const region_gid = makeRegion(&refinements, elem_gid);
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
        .ty = .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } },
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
    const region_gid = makeRegion(&refinements, elem_gid);
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = ptr_gid;
    const state = testState(&ctx, &results, &refinements);

    const struct_type = core.Type{ .@"struct" = &.{
        .type_id = 1234,
        .fields = &.{ .{ .scalar = .{} }, .{ .scalar = .{} } },
        .multiplicity = .region,
        .is_packed = true,
    } };
    try Inst.apply(state, 1, .{ .bitcast = .{
        .src = .{ .inst = 0 },
        .ty = .{ .optional = .{ .to = &.{ .pointer = .{ .to = &struct_type } } } },
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
    const region_gid = makeRegion(&parent, elem_gid);
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
    const branch_region_gid = makeRegion(&branch, branch_elem_gid);
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
    const parent_region_gid = makeRegion(&parent, parent_elem_gid);
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
    try Inst.apply(state, 2, .{ .ret_safe = .{ .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = .{} } } } } });
}

test "divergent allocated pointer branch result does not orphan alternate allocation" {
    var ctx, var parent = initTest();
    defer ctx.deinit();
    defer parent.deinit();

    const alloc_gid = try parent.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const placeholder_elem = try parent.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .placeholder = {} } },
    } });
    const placeholder_region = makeRegion(&parent, placeholder_elem);
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
    const region_gid = makeRegion(refinements, elem_gid);
    return refinements.appendEntity(.{ .pointer = .{ .to = region_gid } });
}

fn expectMemorySafetySet(ref: *const Refinements.Refinement) !void {
    const analyte = switch (ref.*) {
        .scalar => |s| s.analyte,
        .pointer => |p| p.analyte,
        .optional => |o| o.analyte,
        .errorunion => |e| e.analyte,
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
    const region_gid = try refinements.appendEntity(.{
        .scalar = .{
            .multiplicity = .region,
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
    const src_region_gid = makeRegion(&refinements, src_elem_gid);
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
    const dest_region_gid = makeRegion(&refinements, dest_elem_gid);
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = dest_region_gid } });

    // Create freed source region - memory_safety on the region
    const src_region_gid = try refinements.appendEntity(.{
        .scalar = .{
            .multiplicity = .region,
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
    const region_gid = try refinements.appendEntity(.{ .scalar = .{
        .multiplicity = .region,
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
    const src_region_gid = makeRegion(&refinements, src_elem_gid);
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
    const region_gid = try refinements.appendEntity(.{ .scalar = .{
        .multiplicity = .region,
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
    const region_gid = try refinements.appendEntity(.{ .scalar = .{
        .multiplicity = .region,
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
    const dest_region_gid = makeRegion(&refinements, dest_elem_gid);
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = dest_region_gid } });

    const src_elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const src_region_gid = makeRegion(&refinements, src_elem_gid);
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
    const region_gid = try refinements.appendEntity(.{ .scalar = .{ .multiplicity = .region } });
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

test "allocator free treats error_stub slice as no-op" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const alloc_gid = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });
    const region_gid = try refinements.appendEntity(.{ .scalar = .{
        .multiplicity = .region,
        .analyte = .{ .memory_safety = .{ .error_stub = {} } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = region_gid } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = alloc_gid;
    results[1].refinement = ptr_gid;

    const state = testState(&ctx, &results, &refinements);
    try Inst.call(state, 2, null, .{ .void = {} }, &.{
        .{ .inst = 0 },
        .{ .inst = 1 },
    }, "std.mem.Allocator.free");
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
    _ = try refinements.appendEntity(.{ .allocator = .{ .type_id = 100 } });

    // Create a region with .allocated memory_safety (simulating heap allocation)
    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = makeRegion(&refinements, elem_gid);

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

    const nested_type = tag.Type{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } };
    try Inst.apply(state, 0, .{ .block = .{ .ty = nested_type } });

    const ptr_gid = results[0].refinement.?;
    const ptr_ref = refinements.at(ptr_gid).*;
    try std.testing.expectEqual(.pointer, std.meta.activeTag(ptr_ref));

    const ptr_ms = ptr_ref.pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(ptr_ms));

    const region_gid = ptr_ref.pointer.to;
    const region_ref = refinements.at(region_gid).*;
    try std.testing.expectEqual(.region, region_ref.getMultiplicity());
    const region_ms = region_ref.scalar.analyte.memory_safety.?;
    try std.testing.expectEqual(.placeholder, std.meta.activeTag(region_ms));

    const scalar_gid = region_gid;
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
    const region_gid = makeRegion(&refinements, elem_gid);

    var results = [_]Inst{.{}} ** 1;
    results[0].refinement = region_gid;

    var branch1_refinements = try refinements.clone(allocator);
    defer branch1_refinements.deinit();
    var branch2_refinements = try refinements.clone(allocator);
    defer branch2_refinements.deinit();

    branch1_refinements.at(region_gid).scalar.analyte.memory_safety = .{ .interned = ctx.meta };
    branch2_refinements.at(region_gid).scalar.analyte.memory_safety = .{ .interned = ctx.meta };

    var branch1_results = results;
    var branch2_results = results;
    const branches = [_]State{
        testState(&ctx, &branch1_results, &branch1_refinements),
        testState(&ctx, &branch2_results, &branch2_refinements),
    };

    try tag.splatMerge(.cond_br, &results, &ctx, &refinements, &branches, null, null, null);

    try std.testing.expect(refinements.at(region_gid).scalar.analyte.memory_safety != null);
    try std.testing.expect(refinements.at(region_gid).scalar.analyte.memory_safety.? == .interned);
}

test "memcpy propagates region element memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const dest_elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const dest_region_gid = makeRegion(&refinements, dest_elem_gid);
    const dest_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = dest_region_gid,
        .analyte = .{ .memory_safety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } } },
    } });

    const src_elem_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .memory_safety = .{ .interned = ctx.meta } },
    } });
    const src_region_gid = makeRegion(&refinements, src_elem_gid);
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
    const dummy_src = tag.Src{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = .{} } } };

    try Inst.apply(state, 0, .{ .bool_or = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 1, .{ .bool_and = .{ .lhs = dummy_src, .rhs = dummy_src } });

    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(results[0].refinement.?).scalar.analyte.memory_safety.?));
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(results[1].refinement.?).scalar.analyte.memory_safety.?));
}

test "scalar arithmetic cast and unary ops set result memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 29;
    const state = testState(&ctx, &results, &refinements);
    const dummy_src = tag.Src{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = .{} } } };

    try Inst.apply(state, 0, .{ .shr = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 1, .{ .shl = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 2, .{ .mul = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 3, .{ .min = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 4, .{ .not = .{ .src = dummy_src } });
    try Inst.apply(state, 5, .{ .trunc = .{ .src = dummy_src } });
    try Inst.apply(state, 6, .{ .clz = .{ .src = dummy_src } });
    try Inst.apply(state, 7, .{ .bit_or = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 8, .{ .xor = .{ .lhs = dummy_src, .rhs = dummy_src } });
    try Inst.apply(state, 9, .{ .sqrt = .{ .src = dummy_src } });
    try Inst.apply(state, 10, .{ .sin = .{ .src = dummy_src } });
    try Inst.apply(state, 11, .{ .cos = .{ .src = dummy_src } });
    try Inst.apply(state, 12, .{ .tan = .{ .src = dummy_src } });
    try Inst.apply(state, 13, .{ .exp = .{ .src = dummy_src } });
    try Inst.apply(state, 14, .{ .exp2 = .{ .src = dummy_src } });
    try Inst.apply(state, 15, .{ .log = .{ .src = dummy_src } });
    try Inst.apply(state, 16, .{ .log2 = .{ .src = dummy_src } });
    try Inst.apply(state, 17, .{ .log10 = .{ .src = dummy_src } });
    try Inst.apply(state, 18, .{ .floor = .{ .src = dummy_src } });
    try Inst.apply(state, 19, .{ .ceil = .{ .src = dummy_src } });
    try Inst.apply(state, 20, .{ .round = .{ .src = dummy_src } });
    try Inst.apply(state, 21, .{ .trunc_float = .{ .src = dummy_src } });
    try Inst.apply(state, 22, .{ .neg = .{ .src = dummy_src } });
    try Inst.apply(state, 23, .{ .abs = .{ .src = dummy_src } });
    try Inst.apply(state, 24, .{ .popcount = .{ .src = dummy_src } });
    try Inst.apply(state, 25, .{ .byte_swap = .{ .src = dummy_src } });
    try Inst.apply(state, 26, .{ .bit_reverse = .{ .src = dummy_src } });
    try Inst.apply(state, 27, .{ .fpext = .{ .src = dummy_src } });
    try Inst.apply(state, 28, .{ .intcast_safe = .{ .src = dummy_src } });

    for (0..29) |i| {
        try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(results[i].refinement.?).scalar.analyte.memory_safety.?));
    }
}

test "select reduce and predicate queries set result memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 7;
    const state = testState(&ctx, &results, &refinements);
    const dummy_src = tag.Src{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = .{} } } };

    try Inst.apply(state, 0, .{ .select = .{ .mask = dummy_src, .a = dummy_src, .b = dummy_src } });
    try Inst.apply(state, 1, .{ .reduce = .{ .src = .{ .inst = 0 } } });
    try Inst.apply(state, 2, .{ .error_set_has_value = .{ .src = dummy_src } });
    try Inst.apply(state, 3, .{ .is_err = .{ .src = dummy_src } });
    try Inst.apply(state, 4, .{ .is_err_ptr = .{ .src = dummy_src } });
    try Inst.apply(state, 5, .{ .is_non_err_ptr = .{ .src = dummy_src } });
    try Inst.apply(state, 6, .{ .is_named_enum_value = .{ .src = dummy_src } });

    for (0..7) |i| {
        try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(results[i].refinement.?).scalar.analyte.memory_safety.?));
    }
}

test "shift overflow op sets result memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);
    const dummy_src = tag.Src{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = .{} } } };

    try Inst.apply(state, 0, .{ .shl_with_overflow = .{ .lhs = dummy_src, .rhs = dummy_src } });

    const result_gid = results[0].refinement.?;
    const result = refinements.at(result_gid).@"struct";
    try std.testing.expectEqual(.stack, std.meta.activeTag(result.analyte.memory_safety.?));
    for (result.fields) |field_gid| {
        try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(field_gid).scalar.analyte.memory_safety.?));
    }
}

test "store interned pointer initializes destination pointer target memory_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const elem_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const region_gid = makeRegion(&refinements, elem_gid);
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
        .src = .{ .interned = .{ .ip_idx = 1, .ty = .{ .pointer = .{ .to = &.{ .scalar = .{ .multiplicity = .region } } } } } },
    } });

    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(pointer_slot_gid).pointer.analyte.memory_safety.?));
    try std.testing.expectEqual(.interned, std.meta.activeTag(refinements.at(region_gid).scalar.analyte.memory_safety.?));
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
        .src = .{ .interned = .{ .ip_idx = 1, .ty = .{ .@"struct" = &.{ .type_id = 100, .fields = &.{.{ .scalar = .{} }} } } } },
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
    const old_region_gid = makeRegion(&refinements, old_elem_gid);
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
    const new_region_gid = makeRegion(&refinements, new_elem_gid);
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
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(old_region_gid).scalar.analyte.memory_safety.?));
    try std.testing.expectEqual(.allocated, std.meta.activeTag(refinements.at(new_region_gid).scalar.analyte.memory_safety.?));
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
    const dummy_src = tag.Src{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = .{} } } };

    try Inst.apply(state, 0, .{ .aggregate_init = .{
        .ty = .{ .@"struct" = &.{ .type_id = 100, .fields = &.{.{ .scalar = .{} }} } },
        .elements = &.{dummy_src},
    } });

    const struct_gid = results[0].refinement.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(refinements.at(struct_gid).@"struct".analyte.memory_safety.?));
}
