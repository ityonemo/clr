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
        .restrict = .null_safety,
    };
}

test "is_non_null does not modify analyte" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create an optional refinement with no null_safety set
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });

    var results = [_]Inst{.{ .refinement = opt_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // is_non_null is a pure operation - does nothing to the analyte
    try Inst.apply(state, 1, .{ .is_non_null = .{ .src = .{ .inst = 0 } } });

    // null_safety should still be null (unset)
    try std.testing.expect(refinements.at(opt_gid).optional.analyte.null_safety == null);
}

test "is_null does not modify analyte" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create an optional refinement with no null_safety set
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });

    var results = [_]Inst{.{ .refinement = opt_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // is_null is a pure operation - does nothing to the analyte
    try Inst.apply(state, 1, .{ .is_null = .{ .src = .{ .inst = 0 } } });

    // null_safety should still be null (unset)
    try std.testing.expect(refinements.at(opt_gid).optional.analyte.null_safety == null);
}

test "cond_br sets non_null on true branch after is_non_null check" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create an optional with no null_safety set (unchecked)
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });

    // Set up results: inst 0 = optional, inst 1 = is_non_null check
    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = opt_gid;
    results[1].inst_tag = .{ .is_non_null = .{ .src = .{ .inst = 0 } } };
    const state = testState(&ctx, &results, &refinements);

    // cond_br on true branch (branch=true) should set to non_null
    try Inst.apply(state, 2, .{ .cond_br = .{ .condition_idx = 1, .branch = true } });

    const ns = refinements.at(opt_gid).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .non_null);
}

test "cond_br sets null on false branch after is_non_null check" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create an optional with no null_safety set (unchecked)
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });

    // Set up results: inst 0 = optional, inst 1 = is_non_null check
    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = opt_gid;
    results[1].inst_tag = .{ .is_non_null = .{ .src = .{ .inst = 0 } } };
    const state = testState(&ctx, &results, &refinements);

    // cond_br on false branch (branch=false) should set to null
    try Inst.apply(state, 2, .{ .cond_br = .{ .condition_idx = 1, .branch = false } });

    const ns = refinements.at(opt_gid).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .null);
}

test "optional_payload errors on unchecked unwrap" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create an optional with NO null_safety set (unchecked)
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = inner_gid } });

    var results = [_]Inst{.{ .refinement = opt_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // optional_payload should error on unchecked optional
    const result = Inst.apply(state, 1, .{ .optional_payload = .{ .src = .{ .inst = 0 } } });
    try std.testing.expectError(error.UncheckedOptionalUnwrap, result);
}

test "optional_payload errors on known null unwrap" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create an optional with null_safety set to .@"null"
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .null = .{ .function = "", .file = "test.zig", .line = 1, .column = 1 } } },
        .to = inner_gid,
    } });

    var results = [_]Inst{.{ .refinement = opt_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // optional_payload should error on known null
    const result = Inst.apply(state, 1, .{ .optional_payload = .{ .src = .{ .inst = 0 } } });
    try std.testing.expectError(error.NullUnwrap, result);
}

test "optional_payload succeeds on checked non_null" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create an optional with null_safety set to .non_null
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .non_null = .{ .function = "", .file = "test.zig", .line = 1, .column = 1 } } },
        .to = inner_gid,
    } });

    var results = [_]Inst{.{ .refinement = opt_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // optional_payload should succeed on checked non_null
    try Inst.apply(state, 1, .{ .optional_payload = .{ .src = .{ .inst = 0 } } });

    // Result should be a value-copy of the inner entity
    try std.testing.expect(results[1].refinement.? != inner_gid);
}

test "store to optional with null sets null state" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create a pointer to an optional
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = inner_gid } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = opt_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Store null to the optional
    const scalar_type: tag.Type = .{ .scalar = {} };
    const null_type: tag.Type = .{ .null = &scalar_type };
    try Inst.apply(state, 1, .{ .store = .{ .ptr = .{ .inst = 0 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = null_type } } } });

    const ns = refinements.at(opt_gid).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .null);
}

test "store to optional with value sets non_null state" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create a pointer to an optional
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = inner_gid } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = opt_gid } });

    var results = [_]Inst{.{ .refinement = ptr_gid }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Store a runtime value to the optional
    try Inst.apply(state, 1, .{ .store = .{ .ptr = .{ .inst = 0 }, .src = .{ .inst = 1 } } });

    const ns = refinements.at(opt_gid).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .non_null);
}

test "init_global sets null state on optional" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const NullSafety = @import("null_safety.zig").NullSafety;

    // Create an optional (pointee)
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = inner_gid } });
    // Create pointer to the optional (ptr_gid)
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = opt_gid } });

    // Initialize as null global
    const loc = tag.GlobalLocation{ .file = "test.zig", .line = 1, .column = 1 };
    NullSafety.init_global(&refinements, ptr_gid, opt_gid, &ctx, false, true, loc, null);

    const ns = refinements.at(opt_gid).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .null);
    try std.testing.expectEqualStrings("test.zig", ns.null.file);
}

test "init_global sets non_null state on optional" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    const NullSafety = @import("null_safety.zig").NullSafety;

    // Create an optional (pointee)
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = inner_gid } });
    // Create pointer to the optional
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = opt_gid } });

    // Initialize as non-null global (is_null_opt = false)
    const loc = tag.GlobalLocation{ .file = "test.zig", .line = 1, .column = 1 };
    NullSafety.init_global(&refinements, ptr_gid, opt_gid, &ctx, false, false, loc, null);

    const ns = refinements.at(opt_gid).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .non_null);
}

test "valueCopy preserves null_safety on optional" {
    const allocator = std.testing.allocator;

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an inner scalar first
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{} });

    // Create an optional with null_safety set to .@"null"
    const opt_gid = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .null = .{ .function = "", .file = "test.zig", .line = 1, .column = 1 } } },
        .to = inner_gid,
    } });

    // Copy it
    const copy_gid = try refinements.valueCopy(opt_gid);

    // Verify the copy has the same null_safety
    const ns = refinements.at(copy_gid).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .null);
    try std.testing.expectEqualStrings("test.zig", ns.null.file);
}

test "call is no-op for null_safety" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    // null_safety.call returns false (no intercepts)
    const intercepted = try Inst.splatCall(state, 0, .{ .void = {} }, &.{}, "any.function");
    try std.testing.expect(intercepted == false);
}

test "optional_payload succeeds when preceded by is_non_null assertion on same source" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create an optional with NO null_safety set (unchecked)
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .to = inner_gid } });

    // Set up results:
    // inst 0 = the optional value
    // inst 1 = is_non_null assertion (Zig's safety check for .?)
    // inst 2 = optional_payload (the actual unwrap)
    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = opt_gid;
    const state = testState(&ctx, &results, &refinements);

    // Apply is_non_null on inst 0 (this is what Zig generates for .?)
    try Inst.apply(state, 1, .{ .is_non_null = .{ .src = .{ .inst = 0 } } });

    // Now optional_payload should succeed because is_non_null guards it
    // (is_non_null asserts non-null at runtime, so if we get here, it's non-null)
    try Inst.apply(state, 2, .{ .optional_payload = .{ .src = .{ .inst = 0 } } });

    // Result should be a value-copy of the inner entity
    try std.testing.expect(results[2].refinement.? != inner_gid);
}

test "aggregate_init incorporates null_safety from source elements" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create source optionals with known null_safety states
    const inner1 = try refinements.appendEntity(.{ .scalar = .{} });
    const null_opt = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .null = .{ .function = "", .file = "test.zig", .line = 1, .column = 1 } } },
        .to = inner1,
    } });

    const inner2 = try refinements.appendEntity(.{ .scalar = .{} });
    const nonnull_opt = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .non_null = .{ .function = "", .file = "test.zig", .line = 2, .column = 1 } } },
        .to = inner2,
    } });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = null_opt;
    results[1].refinement = nonnull_opt;
    const state = testState(&ctx, &results, &refinements);

    // Create struct with two optional fields using aggregate_init
    const opt_type: tag.Type = .{ .optional = &.{ .scalar = {} } };
    const struct_type = tag.Type{ .@"struct" = &.{
        .type_id = 100,
        .fields = &.{ opt_type, opt_type },
    } };
    const elements = &[_]tag.Src{ .{ .inst = 0 }, .{ .inst = 1 } };
    try Inst.apply(state, 2, .{ .aggregate_init = .{ .ty = struct_type, .elements = elements } });

    // Check the struct's fields
    const struct_gid = results[2].refinement.?;
    const struct_ref = refinements.at(struct_gid);

    const field0_gid = struct_ref.@"struct".fields[0];
    const field1_gid = struct_ref.@"struct".fields[1];

    // Field 0 should have null state
    const field0_ns = refinements.at(field0_gid).optional.analyte.null_safety.?;
    try std.testing.expect(field0_ns == .null);

    // Field 1 should have non_null state
    const field1_ns = refinements.at(field1_gid).optional.analyte.null_safety.?;
    try std.testing.expect(field1_ns == .non_null);
}
