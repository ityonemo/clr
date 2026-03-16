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
        .restrict = .fieldparentptr_safety,
    };
}

test "struct_field_ptr records origin on field pointer" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create a struct with a field - heap allocate fields array
    const field_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(Gid, 1);
    fields[0] = field_gid;
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .fields = fields, .type_id = 100 } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = struct_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // struct_field_ptr should record origin info (type_id must match struct's type_id)
    try Inst.apply(state, 1, .{ .struct_field_ptr = .{ .base = .{ .inst = 0 }, .field_index = 0, .ty = .{ .scalar = {} }, .type_id = 100 } });

    // Check the field pointer has fieldparentptr_safety set
    const field_ptr_gid = results[1].refinement.?;
    const fps = refinements.at(field_ptr_gid).pointer.analyte.fieldparentptr_safety;
    try std.testing.expect(fps != null);
    try std.testing.expectEqual(@as(usize, 0), fps.?.field_index);
    try std.testing.expectEqual(@as(Refinements.Tid, 100), fps.?.container_type_id);
}

test "field_parent_ptr succeeds when origin matches" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create a struct with a field - heap allocate fields array
    const field_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(Gid, 1);
    fields[0] = field_gid;
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .fields = fields, .type_id = 100 } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = struct_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Get field pointer (type_id must match struct's type_id)
    try Inst.apply(state, 1, .{ .struct_field_ptr = .{ .base = .{ .inst = 0 }, .field_index = 0, .ty = .{ .scalar = {} }, .type_id = 100 } });

    // field_parent_ptr with matching container type and field index should succeed
    try Inst.apply(state, 2, .{ .field_parent_ptr = .{ .field_ptr = .{ .inst = 1 }, .type_id = 100, .field_index = 0, .ty = .{ .scalar = {} } } });
}

test "field_parent_ptr errors on wrong field index" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create a struct with a field - heap allocate fields array
    const field_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(Gid, 1);
    fields[0] = field_gid;
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .fields = fields, .type_id = 100 } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = struct_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Get field pointer to field 0 (type_id must match struct's type_id)
    try Inst.apply(state, 1, .{ .struct_field_ptr = .{ .base = .{ .inst = 0 }, .field_index = 0, .ty = .{ .scalar = {} }, .type_id = 100 } });

    // field_parent_ptr with wrong field index should error
    const result = Inst.apply(state, 2, .{ .field_parent_ptr = .{ .field_ptr = .{ .inst = 1 }, .type_id = 100, .field_index = 1, .ty = .{ .scalar = {} } } });
    try std.testing.expectError(error.FieldParentPtrFieldMismatch, result);
}

test "field_parent_ptr errors on non-field pointer" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create a regular pointer with NO fieldparentptr_safety
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = scalar_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // field_parent_ptr with pointer that doesn't have origin info should error
    const result = Inst.apply(state, 1, .{ .field_parent_ptr = .{ .field_ptr = .{ .inst = 0 }, .type_id = 100, .field_index = 0, .ty = .{ .scalar = {} } } });
    try std.testing.expectError(error.InvalidFieldParentPtr, result);
}

test "call is no-op for fieldparentptr_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    // fieldparentptr_safety.call returns false (no intercepts)
    const intercepted = try Inst.splatCall(state, 0, .{ .void = {} }, &.{}, "any.function");
    try std.testing.expect(intercepted == false);
}

test "field_parent_ptr errors on wrong container type" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();
    const allocator = std.testing.allocator;

    // Create a struct with type_id 100
    const field_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const fields = try allocator.alloc(Gid, 1);
    fields[0] = field_gid;
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .fields = fields, .type_id = 100 } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = struct_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Get field pointer (type_id = 100 matches struct)
    try Inst.apply(state, 1, .{ .struct_field_ptr = .{ .base = .{ .inst = 0 }, .field_index = 0, .ty = .{ .scalar = {} }, .type_id = 100 } });

    // field_parent_ptr with wrong type_id (200 != 100) should error
    const result = Inst.apply(state, 2, .{ .field_parent_ptr = .{ .field_ptr = .{ .inst = 1 }, .type_id = 200, .field_index = 0, .ty = .{ .scalar = {} } } });
    try std.testing.expectError(error.FieldParentPtrTypeMismatch, result);
}
