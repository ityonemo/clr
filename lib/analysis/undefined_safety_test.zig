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
        .restrict = .undefined_safety,
    };
}

test "alloc creates pointer with undefined pointee" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Alloc creates a pointer to a scalar
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // Check pointer was created
    const ptr_gid = results[0].refinement.?;
    try std.testing.expectEqual(.pointer, std.meta.activeTag(refinements.at(ptr_gid).*));

    // Check pointee is undefined
    const pointee_gid = refinements.at(ptr_gid).pointer.to;
    const undef = refinements.at(pointee_gid).scalar.analyte.undefined_safety.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
}

test "store with undefined type wrapper keeps state undefined" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // First alloc at instruction 1, then store with .undefined wrapper
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    const scalar_type: tag.Type = .{ .scalar = {} };
    const undef_type: tag.Type = .{ .undefined = &scalar_type };
    try Inst.apply(state, 0, .{ .store = .{ .ptr = .{ .inst = 1 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = undef_type } } } });

    // Check the pointee's undefined state
    const pointee_gid = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_gid).scalar.analyte.undefined_safety.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
}

test "store with defined value sets state to defined" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // First alloc at instruction 1, then store a defined value
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 0, .{ .store = .{ .ptr = .{ .inst = 1 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = {} } } } } });

    // Check the pointee's undefined state
    const pointee_gid = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_gid).scalar.analyte.undefined_safety.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(undef));
}

test "load from undefined value returns error" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Alloc creates undefined pointee
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // Load from undefined should error
    const result = Inst.apply(state, 1, .{ .load = .{ .ptr = .{ .inst = 0 } } });
    try std.testing.expectError(error.UseBeforeAssign, result);
}

test "load from defined value succeeds" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Alloc and store a defined value
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .store = .{ .ptr = .{ .inst = 0 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = {} } } } } });

    // Load from defined should succeed
    try Inst.apply(state, 2, .{ .load = .{ .ptr = .{ .inst = 0 } } });
}

test "init_global sets undefined state on union" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create a union with two fields (all null/inactive initially)
    const fields = try std.testing.allocator.alloc(?Gid, 2);
    @memset(fields, null);
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{ .fields = fields, .type_id = 5120 } });
    // Create pointer to the union (ptr_gid)
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = union_gid } });

    // Initialize as undefined global
    const UndefinedSafety = @import("undefined_safety.zig").UndefinedSafety;
    const loc = tag.GlobalLocation{ .file = "test.zig", .line = 1, .column = 1 };
    UndefinedSafety.init_global(&refinements, ptr_gid, union_gid, &ctx, true, false, loc, null);

    // The union's undefined_safety should be set to .undefined
    const us = refinements.at(union_gid).@"union".analyte.undefined_safety.?;
    try std.testing.expect(us == .undefined);
    try std.testing.expectEqualStrings("test.zig", us.undefined.meta.file);
}

test "semideepCopy preserves union undefined_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create a union with undefined_safety set
    const fields = try std.testing.allocator.alloc(?Gid, 2);
    @memset(fields, null);
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = .{ .function = "", .file = "test.zig", .line = 1, .column = 1 } } } },
        .fields = fields,
        .type_id = 5120,
    } });

    // Copy it
    const copy_gid = try refinements.semideepCopy(union_gid);

    // Verify the copy has the same undefined_safety
    const us = refinements.at(copy_gid).@"union".analyte.undefined_safety.?;
    try std.testing.expect(us == .undefined);
    try std.testing.expectEqualStrings("test.zig", us.undefined.meta.file);

    // Also verify fields are all null (no active field) - preserved from source
    try std.testing.expect(refinements.at(copy_gid).@"union".fields[0] == null);
    try std.testing.expect(refinements.at(copy_gid).@"union".fields[1] == null);
}

test "store with .null to optional sets inner to defined" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Alloc at instruction 0 with optional type, then store null
    const scalar_type: tag.Type = .{ .scalar = {} };
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .optional = &scalar_type } } });

    // Store null to the optional - inner should be defined (null is a valid defined value)
    const null_type: tag.Type = .{ .@"null" = &scalar_type };
    try Inst.apply(state, 1, .{ .store = .{ .ptr = .{ .inst = 0 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = null_type } } } });

    // Check the pointee is an optional
    const pointee_gid = refinements.at(results[0].refinement.?).pointer.to;
    try std.testing.expectEqual(.optional, std.meta.activeTag(refinements.at(pointee_gid).*));

    // Check the optional's inner value is a defined scalar
    const inner_gid = refinements.at(pointee_gid).optional.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(inner_gid).*));
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(inner_gid).scalar.analyte.undefined_safety.?));
}

test "call errors on undefined arg to posix.close" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create undefined scalar (simulating undefined fd)
    const undef_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = undef_gid;

    const state = testState(&ctx, &results, &refinements);

    // posix.close with undefined fd arg should error
    const args = &[_]tag.Src{.{ .inst = 0 }};
    const result = Inst.call(state, 1, null, .{ .void = {} }, args, "std.posix.close");
    try std.testing.expectError(error.UseBeforeAssign, result);
}

test "call succeeds on defined arg to posix.close" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create defined scalar (valid fd)
    const defined_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = defined_gid;

    const state = testState(&ctx, &results, &refinements);

    // posix.close with defined fd arg should succeed (no error)
    const args = &[_]tag.Src{.{ .inst = 0 }};
    try Inst.call(state, 1, null, .{ .void = {} }, args, "std.posix.close");
}

// =============================================================================
// BinOp tests - binary operations should error on undefined operands
// =============================================================================

const binop_tags = .{ .add, .sub, .mul, .bit_and, .bit_or, .xor, .cmp_eq, .cmp_neq, .cmp_gt, .cmp_gte, .cmp_lt, .cmp_lte, .add_with_overflow, .sub_with_overflow, .mul_with_overflow };

test "binop errors on undefined lhs" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const undef_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });
    const defined_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = undef_gid;
    results[1].refinement = defined_gid;

    const state = testState(&ctx, &results, &refinements);

    inline for (binop_tags) |op| {
        const opcode = @unionInit(tag.AnyTag, @tagName(op), .{ .lhs = .{ .inst = 0 }, .rhs = .{ .inst = 1 } });
        const result = Inst.apply(state, 2, opcode);
        try std.testing.expectError(error.UseBeforeAssign, result);
    }
}

test "binop errors on undefined rhs" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const defined_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });
    const undef_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = defined_gid;
    results[1].refinement = undef_gid;

    const state = testState(&ctx, &results, &refinements);

    inline for (binop_tags) |op| {
        const opcode = @unionInit(tag.AnyTag, @tagName(op), .{ .lhs = .{ .inst = 0 }, .rhs = .{ .inst = 1 } });
        const result = Inst.apply(state, 2, opcode);
        try std.testing.expectError(error.UseBeforeAssign, result);
    }
}

// Non-overflow binops produce scalar result
const binop_tags_non_overflow = .{ .add, .sub, .mul, .bit_and, .bit_or, .xor, .cmp_eq, .cmp_neq, .cmp_gt, .cmp_gte, .cmp_lt, .cmp_lte };

test "binop succeeds on defined operands" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const defined1 = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });
    const defined2 = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = defined1;
    results[1].refinement = defined2;

    const state = testState(&ctx, &results, &refinements);

    inline for (binop_tags_non_overflow) |op| {
        const opcode = @unionInit(tag.AnyTag, @tagName(op), .{ .lhs = .{ .inst = 0 }, .rhs = .{ .inst = 1 } });
        try Inst.apply(state, 2, opcode);
    }
}

// =============================================================================
// Overflow operation tests - separate success tests since they create structs
// =============================================================================

const overflow_tags = .{ .add_with_overflow, .sub_with_overflow, .mul_with_overflow };

test "overflow ops succeed on defined operands" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const defined1 = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });
    const defined2 = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = defined1;
    results[1].refinement = defined2;

    const state = testState(&ctx, &results, &refinements);

    inline for (overflow_tags) |op| {
        const opcode = @unionInit(tag.AnyTag, @tagName(op), .{ .lhs = .{ .inst = 0 }, .rhs = .{ .inst = 1 } });
        try Inst.apply(state, 2, opcode);
        // Overflow ops create a struct with two fields
        const struct_gid = results[2].refinement.?;
        const s = refinements.at(struct_gid).@"struct";

        // Both fields should have undefined_safety set to defined
        const field0 = refinements.at(s.fields[0]).scalar;
        const field1 = refinements.at(s.fields[1]).scalar;
        try std.testing.expect(field0.analyte.undefined_safety != null);
        try std.testing.expect(field0.analyte.undefined_safety.? == .defined);
        try std.testing.expect(field1.analyte.undefined_safety != null);
        try std.testing.expect(field1.analyte.undefined_safety.? == .defined);
    }
}

// =============================================================================
// UnOp tests - unary operations should error on undefined operand
// =============================================================================

const unop_tags = .{ .ctz, .not, .trunc };

test "unop errors on undefined operand" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const undef_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = undef_gid;

    const state = testState(&ctx, &results, &refinements);

    inline for (unop_tags) |op| {
        const opcode = @unionInit(tag.AnyTag, @tagName(op), .{ .src = .{ .inst = 0 } });
        const result = Inst.apply(state, 1, opcode);
        try std.testing.expectError(error.UseBeforeAssign, result);
    }
}

test "unop succeeds on defined operand" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const defined_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = defined_gid;

    const state = testState(&ctx, &results, &refinements);

    inline for (unop_tags) |op| {
        const opcode = @unionInit(tag.AnyTag, @tagName(op), .{ .src = .{ .inst = 0 } });
        try Inst.apply(state, 1, opcode);
    }
}

test "aggregate_init incorporates undefined state from source elements" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 4;
    const state = testState(&ctx, &results, &refinements);

    // Create source scalars - one defined, one undefined
    const defined_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });
    const undefined_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = .{ .function = "test", .file = "test.zig", .line = 1, .column = 1 } } } },
    } });
    results[0].refinement = defined_gid;
    results[1].refinement = undefined_gid;

    // Create struct with two scalar fields using aggregate_init
    const struct_type = tag.Type{ .@"struct" = &.{
        .type_id = 100,
        .fields = &.{ .{ .scalar = {} }, .{ .scalar = {} } },
    } };
    const elements = &[_]tag.Src{ .{ .inst = 0 }, .{ .inst = 1 } };
    try Inst.apply(state, 2, .{ .aggregate_init = .{ .ty = struct_type, .elements = elements } });

    // Check the struct's fields
    const struct_gid = results[2].refinement.?;
    const struct_ref = refinements.at(struct_gid);
    const field0_gid = struct_ref.@"struct".fields[0];
    const field1_gid = struct_ref.@"struct".fields[1];

    // Field 0 should be defined (from defined source)
    const field0_undef = refinements.at(field0_gid).scalar.analyte.undefined_safety.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(field0_undef));

    // Field 1 should be undefined (from undefined source)
    const field1_undef = refinements.at(field1_gid).scalar.analyte.undefined_safety.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(field1_undef));
}

test "get_union_tag on undefined union marks result as undefined" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create a union with undefined_safety = .undefined
    const fields = try std.testing.allocator.alloc(?Gid, 2);
    @memset(fields, null);
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = .{ .function = "test", .file = "test.zig", .line = 1, .column = 1 } } } },
        .fields = fields,
        .type_id = 100,
    } });

    // Put union in results[0] as if it were loaded
    results[0].refinement = union_gid;

    // get_union_tag should succeed but mark result as undefined
    try Inst.apply(state, 1, .{ .get_union_tag = .{ .operand = 0 } });

    // Result should be a scalar marked as undefined
    const result_gid = results[1].refinement.?;
    const result_ref = refinements.at(result_gid);
    try std.testing.expectEqual(.scalar, std.meta.activeTag(result_ref.*));
    try std.testing.expectEqual(.undefined, std.meta.activeTag(result_ref.scalar.analyte.undefined_safety.?));
}

test "get_union_tag on union with no variant_safety marks result as undefined" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create a union with defined undefined_safety but no variant_safety
    const fields = try std.testing.allocator.alloc(?Gid, 2);
    @memset(fields, null);
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
        .fields = fields,
        .type_id = 100,
    } });

    // Put union in results[0]
    results[0].refinement = union_gid;

    // get_union_tag should succeed but mark result as undefined
    try Inst.apply(state, 1, .{ .get_union_tag = .{ .operand = 0 } });

    // Result should be a scalar marked as undefined
    const result_gid = results[1].refinement.?;
    const result_ref = refinements.at(result_gid);
    try std.testing.expectEqual(.scalar, std.meta.activeTag(result_ref.*));
    try std.testing.expectEqual(.undefined, std.meta.activeTag(result_ref.scalar.analyte.undefined_safety.?));
}

test "get_union_tag on union with empty active_metas marks result as undefined" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create a union with variant_safety where all active_metas are null
    const fields = try std.testing.allocator.alloc(?Gid, 2);
    @memset(fields, null);
    const active_metas = try std.testing.allocator.alloc(?@import("../core.zig").Meta, 2);
    @memset(active_metas, null);

    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{
            .undefined_safety = .{ .defined = {} },
            .variant_safety = .{ .active_metas = active_metas },
        },
        .fields = fields,
        .type_id = 100,
    } });

    // Put union in results[0]
    results[0].refinement = union_gid;

    // get_union_tag should succeed but mark result as undefined
    try Inst.apply(state, 1, .{ .get_union_tag = .{ .operand = 0 } });

    // Result should be a scalar marked as undefined
    const result_gid = results[1].refinement.?;
    const result_ref = refinements.at(result_gid);
    try std.testing.expectEqual(.scalar, std.meta.activeTag(result_ref.*));
    try std.testing.expectEqual(.undefined, std.meta.activeTag(result_ref.scalar.analyte.undefined_safety.?));
}

test "get_union_tag on union with active variant succeeds" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create a field scalar for the active variant
    const field_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });

    // Create a union with an active variant
    const fields = try std.testing.allocator.alloc(?Gid, 2);
    fields[0] = field_gid; // First variant is active
    fields[1] = null;
    const active_metas = try std.testing.allocator.alloc(?@import("../core.zig").Meta, 2);
    active_metas[0] = .{ .function = "test", .file = "test.zig", .line = 1, .column = 1 };
    active_metas[1] = null;

    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{
            .undefined_safety = .{ .defined = {} },
            .variant_safety = .{ .active_metas = active_metas },
        },
        .fields = fields,
        .type_id = 100,
    } });

    // Put union in results[0]
    results[0].refinement = union_gid;

    // get_union_tag should succeed because a variant is active
    try Inst.apply(state, 1, .{ .get_union_tag = .{ .operand = 0 } });

    // Result should be a defined scalar
    const result_gid = results[1].refinement.?;
    const result_ref = refinements.at(result_gid);
    try std.testing.expectEqual(.scalar, std.meta.activeTag(result_ref.*));
    try std.testing.expectEqual(.defined, std.meta.activeTag(result_ref.scalar.analyte.undefined_safety.?));
}

// =============================================================================
// Packed struct field tests - RMW pattern handling
// =============================================================================

test "load with is_packed_rmw skips undefined check and marks result defined" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Alloc creates pointer to undefined scalar (simulating packed struct backing byte)
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // Verify pointee is undefined
    const ptr_gid = results[0].refinement.?;
    const pointee_gid = refinements.at(ptr_gid).pointer.to;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(refinements.at(pointee_gid).scalar.analyte.undefined_safety.?));

    // Load with is_packed_rmw=true should NOT error even though pointee is undefined
    try Inst.apply(state, 1, .{ .load = .{ .ptr = .{ .inst = 0 }, .is_packed_rmw = true } });

    // Result should be marked as defined
    const result_gid = results[1].refinement.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(result_gid).scalar.analyte.undefined_safety.?));
}

test "load through packed_field pointer errors on undefined field" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create a packed struct with undefined fields
    const fields = try std.testing.allocator.alloc(Gid, 2);
    fields[0] = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });
    fields[1] = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = fields,
        .type_id = 100,
    } });

    // Create a packed_field pointer to field 1 (which is undefined)
    const ptr_gid = try refinements.createPointerToPackedField(fields[1], .{}, .{
        .container_gid = struct_gid,
        .field_index = 1,
    });
    results[0].refinement = ptr_gid;

    // Load through packed_field pointer should error because field 1 is undefined
    const result = Inst.apply(state, 1, .{ .load = .{ .ptr = .{ .inst = 0 } } });
    try std.testing.expectError(error.UseBeforeAssign, result);
}

test "load through packed_field pointer succeeds on defined field" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create a packed struct with field 1 defined
    const fields = try std.testing.allocator.alloc(Gid, 2);
    fields[0] = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });
    fields[1] = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = fields,
        .type_id = 100,
    } });

    // Create a packed_field pointer to field 1 (which is defined)
    const ptr_gid = try refinements.createPointerToPackedField(fields[1], .{}, .{
        .container_gid = struct_gid,
        .field_index = 1,
    });
    results[0].refinement = ptr_gid;

    // Load through packed_field pointer should succeed because field 1 is defined
    try Inst.apply(state, 1, .{ .load = .{ .ptr = .{ .inst = 0 } } });

    // Result should be marked as defined
    const result_gid = results[1].refinement.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(result_gid).scalar.analyte.undefined_safety.?));
}

test "store through packed_field pointer marks only that field as defined" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 4;
    const state = testState(&ctx, &results, &refinements);

    // Create a packed struct with all undefined fields
    const fields = try std.testing.allocator.alloc(Gid, 3);
    fields[0] = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });
    fields[1] = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });
    fields[2] = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = ctx.meta } } },
    } });
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = fields,
        .type_id = 100,
    } });

    // Create a packed_field pointer to field 1
    const ptr_gid = try refinements.createPointerToPackedField(fields[1], .{}, .{
        .container_gid = struct_gid,
        .field_index = 1,
    });
    results[0].refinement = ptr_gid;

    // Create a defined scalar as the source value
    const src_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} } },
    } });
    results[1].refinement = src_gid;

    // Store through packed_field pointer
    try Inst.apply(state, 2, .{ .store = .{ .ptr = .{ .inst = 0 }, .src = .{ .inst = 1 } } });

    // Field 0 should still be undefined
    try std.testing.expectEqual(.undefined, std.meta.activeTag(refinements.at(fields[0]).scalar.analyte.undefined_safety.?));

    // Field 1 should now be defined
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(fields[1]).scalar.analyte.undefined_safety.?));

    // Field 2 should still be undefined
    try std.testing.expectEqual(.undefined, std.meta.activeTag(refinements.at(fields[2]).scalar.analyte.undefined_safety.?));
}
