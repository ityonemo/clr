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
        .restrict = .fd_safety,
    };
}

test "ret_safe does not modify fd_safety state (connectivity tracking handles leaks)" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create a scalar with fd_safety.open state
    const FdSafety = @import("fd_safety.zig").FdSafety;
    const fd_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .fd_safety = FdSafety{ .open = .{ .meta = .{ .function = "test", .file = "test.zig", .line = 1, .column = 1 }, .fd_type = .file } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = fd_gid;

    const state = testState(&ctx, &results, &refinements);

    // ret_safe should NOT modify fd_safety - connectivity tracking handles leak detection
    try Inst.apply(state, 1, .{ .ret_safe = .{ .src = .{ .inst = 0 } } });

    // Check the fd state is unchanged (still open, not closed)
    const fd_ref = refinements.at(fd_gid);
    const fd_safety = fd_ref.scalar.analyte.fd_safety.?;
    try std.testing.expect(fd_safety == .open);
    try std.testing.expect(fd_safety.open.closed == null);
}

test "call intercepts posix.open and sets fd_safety.open state" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;

    const state = testState(&ctx, &results, &refinements);

    // Call posix.open - Inst.call creates the return structure
    const scalar_type: tag.Type = .{ .scalar = {} };
    const return_type: tag.Type = .{ .errorunion = &scalar_type };
    try Inst.call(state, 0, null, return_type, &.{}, "std.posix.open");

    // Check the scalar (fd_t) has fd_safety.open state
    const eu_gid = results[0].refinement.?;
    const scalar_gid = refinements.at(eu_gid).errorunion.to;
    const fd_safety = refinements.at(scalar_gid).scalar.analyte.fd_safety.?;
    try std.testing.expect(fd_safety == .open);
    try std.testing.expect(fd_safety.open.fd_type == .file);
}

test "call intercepts posix.close and marks fd as closed" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create fd with open state
    const FdSafety = @import("fd_safety.zig").FdSafety;
    const fd_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .fd_safety = FdSafety{ .open = .{ .meta = ctx.meta, .fd_type = .file } } },
    } });

    var results = [_]Inst{.{}} ** 2;
    results[0].refinement = fd_gid;

    const state = testState(&ctx, &results, &refinements);

    // Call posix.close with fd as arg
    const args = &[_]tag.Src{.{ .inst = 0 }};
    try Inst.call(state, 1, null, .{ .void = {} }, args, "std.posix.close");

    // Check the fd is marked as closed (open.closed is not null)
    const fd_safety = refinements.at(fd_gid).scalar.analyte.fd_safety.?;
    try std.testing.expect(fd_safety == .open);
    try std.testing.expect(fd_safety.open.closed != null);
}

test "call intercepts posix.socket and sets socket fd_type" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;

    const state = testState(&ctx, &results, &refinements);

    // Call posix.socket - Inst.call creates the return structure
    const scalar_type: tag.Type = .{ .scalar = {} };
    const return_type: tag.Type = .{ .errorunion = &scalar_type };
    try Inst.call(state, 0, null, return_type, &.{}, "std.posix.socket");

    // Check the fd has socket type
    const eu_gid = results[0].refinement.?;
    const scalar_gid = refinements.at(eu_gid).errorunion.to;
    const fd_safety = refinements.at(scalar_gid).scalar.analyte.fd_safety.?;
    try std.testing.expect(fd_safety == .open);
    try std.testing.expect(fd_safety.open.fd_type == .socket);
}

test "aggregate_init incorporates fd_safety state from source elements" {
    var ctx, var refinements = initTest();
    defer ctx.deinit();
    defer refinements.deinit();

    // Create source scalars - one with fd_safety set
    const FdSafety = @import("fd_safety.zig").FdSafety;
    const fd_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .fd_safety = FdSafety{ .open = .{ .meta = .{ .function = "test", .file = "test.zig", .line = 1, .column = 1 }, .fd_type = .file } } },
    } });
    const other_gid = try refinements.appendEntity(.{ .scalar = .{} });

    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = fd_gid;
    results[1].refinement = other_gid;

    const state = testState(&ctx, &results, &refinements);

    // Create struct with the fd and other scalar
    const struct_type = tag.Type{ .@"struct" = &.{
        .type_id = 100,
        .fields = &.{ .{ .scalar = {} }, .{ .scalar = {} } },
    } };
    const elements = &[_]tag.Src{ .{ .inst = 0 }, .{ .inst = 1 } };
    try Inst.apply(state, 2, .{ .aggregate_init = .{ .ty = struct_type, .elements = elements } });

    // Check the struct's first field has fd_safety
    const struct_gid = results[2].refinement.?;
    const struct_ref = refinements.at(struct_gid);
    const field0_gid = struct_ref.@"struct".fields[0];
    const field0_fd = refinements.at(field0_gid).scalar.analyte.fd_safety;
    try std.testing.expect(field0_fd != null);
    try std.testing.expect(field0_fd.? == .open);
}
