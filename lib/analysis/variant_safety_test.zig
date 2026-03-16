const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;
const tag = @import("../tag.zig");
const Gid = Refinements.Gid;
const Meta = @import("../core.zig").Meta;
const VariantSafety = @import("variant_safety.zig").VariantSafety;

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
        .restrict = .variant_safety,
    };
}

test "set_union_tag sets active variant" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create a union with two fields (no active variant initially)
    const field0_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const field1_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(?Gid, 2);
    fields[0] = field0_gid;
    fields[1] = field1_gid;
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{ .fields = fields, .type_id = 0 } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = union_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // set_union_tag should update the active variant
    try Inst.apply(state, 1, .{ .set_union_tag = .{ .ptr = .{ .inst = 0 }, .field_index = 1, .ty = .{ .scalar = {} } } });

    // The union's active_metas should show field 1 active, field 0 inactive
    const union_ref = refinements.at(union_gid);
    const vs = union_ref.@"union".analyte.variant_safety.?;
    try std.testing.expect(vs.active_metas[0] == null);
    try std.testing.expect(vs.active_metas[1] != null);
}

test "struct_field_ptr allows access to active variant" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create active_metas array - field 0 is active
    const active_metas = try allocator.alloc(?Meta, 2);
    active_metas[0] = .{ .function = "test", .file = "test.zig", .line = 1, .column = null };
    active_metas[1] = null;

    // Create a union with field 0 active
    const field0_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(?Gid, 2);
    fields[0] = field0_gid;
    fields[1] = null;
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .fields = fields,
        .type_id = 0,
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = union_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Accessing the active field should succeed
    try Inst.apply(state, 1, .{ .struct_field_ptr = .{ .base = .{ .inst = 0 }, .field_index = 0, .ty = .{ .scalar = {} } } });
}

test "struct_field_ptr errors on inactive variant" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create active_metas array - field 0 is active
    const active_metas = try allocator.alloc(?Meta, 2);
    active_metas[0] = .{ .function = "test", .file = "test.zig", .line = 1, .column = null };
    active_metas[1] = null;

    // Create a union with field 0 active
    const field0_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(?Gid, 2);
    fields[0] = field0_gid;
    fields[1] = null;
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .fields = fields,
        .type_id = 0,
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = union_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Accessing inactive field (field 1) should error
    // ty is a pointer type because struct_field_ptr returns pointer to field
    const scalar_type: tag.Type = .{ .scalar = {} };
    const result = Inst.apply(state, 1, .{ .struct_field_ptr = .{ .base = .{ .inst = 0 }, .field_index = 1, .ty = .{ .pointer = &scalar_type } } });
    try std.testing.expectError(error.InactiveVariantAccess, result);
}

test "struct_field_val errors on inactive variant" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create active_metas array - field 2 is active
    const active_metas = try allocator.alloc(?Meta, 3);
    active_metas[0] = null;
    active_metas[1] = null;
    active_metas[2] = .{ .function = "test", .file = "test.zig", .line = 1, .column = null };

    // Create a union with field 2 active
    const field2_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(?Gid, 3);
    fields[0] = null;
    fields[1] = null;
    fields[2] = field2_gid;
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .fields = fields,
        .type_id = 0,
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
    } });

    var results = [_]Inst{.{ .refinement = union_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Accessing inactive field 0 via struct_field_val should error
    const result = Inst.apply(state, 1, .{ .struct_field_val = .{ .operand = 0, .field_index = 0, .ty = .{ .scalar = {} } } });
    try std.testing.expectError(error.InactiveVariantAccess, result);
}

test "struct_field_ptr errors on ambiguous variant after merge" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create variant_safety with TWO fields active (ambiguous state after merge)
    const active_metas = try allocator.alloc(?Meta, 3);
    active_metas[0] = .{ .function = "branch1", .file = "test.zig", .line = 1, .column = null };
    active_metas[1] = .{ .function = "branch2", .file = "test.zig", .line = 2, .column = null };
    active_metas[2] = null;

    // Create fields array
    const field0_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const field1_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(?Gid, 3);
    fields[0] = field0_gid;
    fields[1] = field1_gid;
    fields[2] = null;
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .fields = fields,
        .type_id = 0,
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = union_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Access any field when ambiguous - should error
    const result = Inst.apply(state, 1, .{ .struct_field_ptr = .{ .base = .{ .inst = 0 }, .field_index = 0, .ty = .{ .scalar = {} } } });
    try std.testing.expectError(error.AmbiguousVariantAccess, result);
}

test "cond_br is no-op for variant_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // cond_br should succeed without modifying anything
    try Inst.apply(state, 1, .{ .cond_br = .{ .branch = true, .condition_idx = 0 } });
}

test "switch_br sets active variant" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create active_metas array - multiple fields potentially active (ambiguous)
    const active_metas = try allocator.alloc(?Meta, 2);
    active_metas[0] = .{ .function = "branch1", .file = "test.zig", .line = 1, .column = null };
    active_metas[1] = .{ .function = "branch2", .file = "test.zig", .line = 2, .column = null };

    // Create a union with ambiguous variant
    const field0_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const field1_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(?Gid, 2);
    fields[0] = field0_gid;
    fields[1] = field1_gid;
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .fields = fields,
        .type_id = 0,
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
    } });

    var results = [_]Inst{.{ .refinement = union_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // switch_br with field_index 1 should set only field 1 as active
    try Inst.apply(state, 1, .{ .switch_br = .{
        .case_index = 1,
        .num_cases = 2,
        .union_tag = .{ .union_inst = 0, .field_index = 1 },
    } });

    // Check variant_safety was updated - only field 1 should be active
    const union_ref = refinements.at(union_gid);
    const vs = union_ref.@"union".analyte.variant_safety.?;
    try std.testing.expect(vs.active_metas[0] == null); // field 0 no longer active
    try std.testing.expect(vs.active_metas[1] != null); // field 1 is active
}

test "call is no-op for variant_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    // variant_safety.call returns false (no intercepts)
    const intercepted = try Inst.splatCall(state, 0, .{ .void = {} }, &.{}, "any.function");
    try std.testing.expect(intercepted == false);
}

test "aggregate_init incorporates variant_safety state from source elements" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create a union with variant 0 active
    const active_metas = try allocator.alloc(?Meta, 2);
    active_metas[0] = .{ .function = "test", .file = "test.zig", .line = 1, .column = null };
    active_metas[1] = null;

    const field0_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(?Gid, 2);
    fields[0] = field0_gid;
    fields[1] = null;
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .fields = fields,
        .type_id = 200,
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = union_gid;

    const state = testState(&ctx, &results, &refinements);

    // Create struct with union field using aggregate_init
    const struct_type = tag.Type{ .@"struct" = &.{
        .type_id = 100,
        .fields = &.{ .{ .@"union" = &.{ .type_id = 200, .variants = &.{ .{ .scalar = {} }, .{ .scalar = {} } } } } },
    } };
    const elements = &[_]tag.Src{ .{ .inst = 0 } };
    try Inst.apply(state, 1, .{ .aggregate_init = .{ .ty = struct_type, .elements = elements } });

    // Check the struct's field has variant_safety
    const struct_gid = results[1].refinement.?;
    const struct_ref = refinements.at(struct_gid);
    const field_gid = struct_ref.@"struct".fields[0];
    const field_ref = refinements.at(field_gid);
    try std.testing.expect(field_ref.@"union".analyte.variant_safety != null);
    const vs = field_ref.@"union".analyte.variant_safety.?;
    try std.testing.expect(vs.active_metas[0] != null); // variant 0 active
    try std.testing.expect(vs.active_metas[1] == null); // variant 1 not active
}
