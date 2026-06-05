const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Analyte = @import("../Analyte.zig");
const Gid = Refinements.Gid;
const core = @import("../core.zig");
const Meta = core.Meta;
const tag = @import("../tag.zig");
const gates = @import("gates.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

// strategy:
// FdSafety effectively tracks a list of file descriptor create points.
// Each analyte holds a reference to an index into this list.  Only
// "scalar" refinements may have an FdSafety analyte.  If an fd is reinterpreted
// as any other datatype it should trigger a failure.
//
// safety:
// prevent double-close.
// prevent use-after-close.
//
// leaks:
// when a refinement with a file descriptor goes out of scope, we check to see if
// it's closed.  If it's not closed and no other refinements in the table hold on
// to the same reference, then we trigger a leak warning.
//
// aliasing:  not implemented yet.

pub const FdType = enum {
    file, // posix.open/openat
    socket, // posix.socket/accept
    pipe, // posix.pipe/pipe2
    epoll, // posix.epoll_create
    dup, // posix.dup/dup2
    // note: leakage of stdio is tolerated.
    stdio, // any function that returns stdin or stdout.
};

pub const Open = struct {
    meta: Meta, // Where fd was opened
    fd_type: FdType, // Type of fd (file, socket, pipe, etc.)
};

pub const Closed = struct {
    meta: Meta,
};

pub const TrackedFd = struct {
    opened: Open,
    closed: ?Closed = null,
};

const FdRef = usize;
var tracked: std.ArrayListUnmanaged(TrackedFd) = .empty;
var list_allocator: std.mem.Allocator = undefined;
var initialized = false;

pub const FdSafety = struct {
    ref: FdRef,

    pub fn initModule(allocator: std.mem.Allocator) !void {
        list_allocator = allocator;
        if (initialized) {
            tracked.clearRetainingCapacity();
        } else {
            tracked = .empty;
            initialized = true;
        }
    }

    pub fn deinitModule() void {
        if (!initialized) return;
        tracked.deinit(list_allocator);
        tracked = .empty;
        initialized = false;
    }

    pub fn createForTest(open: Open) !FdSafety {
        return try createTracked(open);
    }

    pub fn getForTest(self: FdSafety) TrackedFd {
        return getTracked(self.ref).*;
    }

    /// Trivial copy - no heap allocations to duplicate.
    pub fn copy(self: @This(), allocator: std.mem.Allocator) error{OutOfMemory}!@This() {
        _ = allocator;
        return self;
    }

    /// Hash this analysis state for memoization.
    pub fn hash(self: @This(), hasher: *std.hash.Wyhash) void {
        hasher.update(std.mem.asBytes(&self.ref));
        const fd = getTrackedOrNull(self.ref) orelse return;
        hasher.update(&.{@as(u8, if (fd.closed != null) 1 else 0)});
    }

    /// Runtime call filter for fd operations.
    /// Intercepts posix.open, posix.close, etc.
    pub fn call(
        state: State,
        index: usize,
        return_type: tag.Type,
        args: []const tag.Src,
        fqn: []const u8,
    ) anyerror!bool {
        _ = return_type;

        // posix.open/openat returns fd_t!OpenError
        if (gates.isPosixOpen(fqn) or gates.isPosixOpenat(fqn)) {
            try handleFdOpen(state, index, .file);
            return true;
        }

        // posix.socket returns socket_t!SocketError
        if (gates.isPosixSocket(fqn)) {
            try handleFdOpen(state, index, .socket);
            return true;
        }

        // posix.accept returns socket_t!AcceptError
        if (gates.isPosixAccept(fqn)) {
            try handleFdOpen(state, index, .socket);
            return true;
        }

        // posix.epoll_create returns fd_t!EpollCreateError
        if (gates.isPosixEpollCreate(fqn)) {
            try handleFdOpen(state, index, .epoll);
            return true;
        }

        // posix.dup returns fd_t!DupError
        if (gates.isPosixDup(fqn)) {
            try handleFdOpen(state, index, .dup);
            return true;
        }

        // posix.dup2 returns void!DupError and replaces the destination fd.
        if (gates.isPosixDup2(fqn)) {
            try handleFdDup2(state, args);
            return true;
        }

        // posix.pipe returns struct with read/write fds
        if (gates.isPosixPipe(fqn)) {
            try handlePipeOpen(state, index);
            return true;
        }

        // posix.close takes fd and returns void
        if (gates.isPosixClose(fqn)) {
            try handleFdClose(state, index, args);
            return true;
        }

        // posix.read/write/etc. use fd - check for use-after-close
        if (gates.isPosixRead(fqn) or gates.isPosixWrite(fqn) or
            gates.isPosixPread(fqn) or gates.isPosixPwrite(fqn))
        {
            try checkFdUse(state, args);
            // Intercept to avoid diving into stdlib syscall code
            return true;
        }

        // posix.flock uses fd for file locking - intercept to avoid syscall code
        if (gates.isPosixFlock(fqn)) {
            try checkFdUse(state, args);
            return true;
        }

        return false;
    }

    /// Handle posix.open/openat/socket/accept/etc. calls.
    /// Creates fd refinement (scalar) with open state.
    fn handleFdOpen(state: State, index: usize, fd_type: FdType) !void {
        // Result is errorunion -> scalar (fd_t is i32)
        const eu_idx = state.results[index].refinement orelse
            std.debug.panic("fd_safety.handleFdOpen: result instruction {d} has no refinement", .{index});
        const eu_ref = state.refinements.at(eu_idx);
        if (eu_ref.* != .errorunion) {
            std.debug.panic("fd_safety.handleFdOpen: expected errorunion result, got {s}", .{@tagName(eu_ref.*)});
        }

        const fd_idx = eu_ref.errorunion.to;
        const fd_ref = state.refinements.at(fd_idx);
        if (fd_ref.* != .scalar) {
            std.debug.panic("fd_safety.handleFdOpen: expected scalar fd payload, got {s}", .{@tagName(fd_ref.*)});
        }

        fd_ref.scalar.analyte.fd_safety = try createTracked(.{
            .meta = state.ctx.meta,
            .fd_type = fd_type,
        });
    }

    /// Handle posix.dup2 calls. dup2 returns void on success, so fd state belongs
    /// on the second argument rather than the call result.
    fn handleFdDup2(state: State, args: []const tag.Src) !void {
        if (args.len < 2) return;

        const dst_idx: Gid = switch (args[1]) {
            .inst => |inst| state.results[inst].refinement orelse
                std.debug.panic("fd_safety.handleFdDup2: destination fd instruction {d} has no refinement", .{inst}),
            .interned, .fnptr => return,
        };

        const dst_ref = state.refinements.at(dst_idx);
        if (dst_ref.* != .scalar) return;

        if (dst_ref.scalar.analyte.fd_safety) |old_fd_safety| {
            const old_fd = getTracked(old_fd_safety.ref);
            if (old_fd.closed == null) {
                old_fd.closed = .{ .meta = state.ctx.meta };
            }
        }

        dst_ref.scalar.analyte.fd_safety = try createTracked(.{
            .meta = state.ctx.meta,
            .fd_type = .dup,
        });
    }

    /// Handle posix.pipe calls.
    /// pipe returns a struct with read_fd and write_fd, both need tracking.
    fn handlePipeOpen(state: State, index: usize) !void {
        const result_idx = state.results[index].refinement orelse
            std.debug.panic("fd_safety.handlePipeOpen: result instruction {d} has no refinement", .{index});
        const result_ref = state.refinements.at(result_idx);

        // If it's an errorunion, follow to the payload
        const payload_idx = if (result_ref.* == .errorunion)
            result_ref.errorunion.to
        else
            result_idx;

        const payload_ref = state.refinements.at(payload_idx);

        // If it's a struct with 2 fields (read_fd, write_fd), mark each
        if (payload_ref.* == .@"struct") {
            for (payload_ref.@"struct".fields) |field_gid| {
                const field_ref = state.refinements.at(field_gid);
                if (field_ref.* == .scalar) {
                    field_ref.scalar.analyte.fd_safety = try createTracked(.{
                        .meta = state.ctx.meta,
                        .fd_type = .pipe,
                    });
                }
            }
        } else {
            std.debug.panic("fd_safety.handlePipeOpen: expected pipe payload struct, got {s}", .{@tagName(payload_ref.*)});
        }
    }

    /// Handle posix.close calls.
    /// Checks for double-close and marks fd as closed.
    fn handleFdClose(state: State, index: usize, args: []const tag.Src) !void {
        _ = index; // close returns void

        // close signature: close(fd) -> args[0]=fd
        if (args.len < 1) return;

        // Get fd refinement
        const fd_idx: Gid = switch (args[0]) {
            .inst => |inst| state.results[inst].refinement orelse
                std.debug.panic("fd_safety.handleFdClose: fd instruction {d} has no refinement", .{inst}),
            .interned, .fnptr => return, // Can't track interned fds
        };

        const fd_ref = state.refinements.at(fd_idx);
        if (fd_ref.* != .scalar) return;

        const fd_safety = fd_ref.scalar.analyte.fd_safety orelse return;
        const fd_state = getTracked(fd_safety.ref);

        // Check for double-close
        if (fd_state.closed) |prev_close| {
            return reportDoubleClose(state.ctx, fd_state.opened, prev_close);
        }

        // Mark as closed
        fd_state.closed = .{ .meta = state.ctx.meta };
    }

    /// Check fd arguments for use-after-close.
    fn checkFdUse(state: State, args: []const tag.Src) !void {
        // First arg is typically the fd
        if (args.len < 1) return;

        const fd_idx: Gid = switch (args[0]) {
            .inst => |inst| state.results[inst].refinement orelse
                std.debug.panic("fd_safety.checkFdUse: fd instruction {d} has no refinement", .{inst}),
            .interned, .fnptr => return,
        };

        const fd_ref = state.refinements.at(fd_idx);
        if (fd_ref.* != .scalar) return;

        const fd_safety = fd_ref.scalar.analyte.fd_safety orelse return;
        const fd_state = getTracked(fd_safety.ref);

        // Check for use-after-close
        if (fd_state.closed) |close_site| {
            return reportUseAfterClose(state.ctx, fd_state.opened, close_site);
        }
    }

    /// Handle store - propagate fd_safety state from source to destination.
    /// This is critical for tracking FDs stored into struct fields.
    pub fn store(state: State, index: usize, params: tag.Store) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;

        // Get pointer GID based on ptr type
        const ptr_gid: ?Gid = switch (params.ptr) {
            .inst => |ptr| results[ptr].refinement orelse
                std.debug.panic("fd_safety.store: ptr instruction {d} has no refinement", .{ptr}),
            .interned => |interned| refinements.getGlobal(interned.ip_idx),
            .fnptr => null,
        };
        if (ptr_gid == null) return;

        const ptr_ref = refinements.at(ptr_gid.?);
        if (ptr_ref.* != .pointer) return;
        const pointee_gid = ptr_ref.pointer.to;

        // Copy fd_safety from source to destination
        copyFdSafetyState(state, pointee_gid, params.src);
    }

    /// Handle ret_safe - with connectivity tracking, no action needed.
    /// FDs reachable from return value are automatically not detected as leaks.
    pub fn ret_safe(state: State, index: usize, params: tag.RetSafe) !void {
        _ = state;
        _ = index;
        _ = params;
    }

    /// Handle ret_load - with connectivity tracking, no action needed.
    /// FDs reachable from return value are automatically not detected as leaks.
    pub fn ret_load(state: State, index: usize, params: tag.RetLoad) !void {
        _ = state;
        _ = index;
        _ = params;
    }

    /// Mark fd as returned (ownership transferred to caller).
    /// Called after receiving a return value from a function call.
    /// With connectivity tracking, no action is needed - fd's reachable from
    /// the return value are automatically not detected as leaks by the caller.
    pub fn call_return(refinements: *Refinements, return_gid: Gid) void {
        _ = refinements;
        _ = return_gid;
    }

    /// End-of-function checks - detect fd leaks.
    pub fn onFinish(results: []Inst, ctx: *Context, refinements: *Refinements, return_gid: Gid) !void {
        var returned_fds = std.AutoHashMap(FdRef, void).init(ctx.allocator);
        defer returned_fds.deinit();
        collectReachableFds(refinements, return_gid, &returned_fds);

        // Check for fd leaks (open fds not closed, not returned)
        for (results) |inst| {
            const idx = inst.refinement orelse continue;

            // Skip arg instructions - fds passed in as arguments are not our
            // responsibility to close, they belong to the caller
            if (inst.inst_tag) |inst_tag| {
                if (std.meta.activeTag(inst_tag) == .arg) continue;
            }

            try checkFdLeakRecursive(refinements, idx, ctx, &returned_fds);
        }
    }

    fn checkFdLeakRecursive(refinements: *Refinements, gid: Gid, ctx: *Context, returned_fds: *std.AutoHashMap(FdRef, void)) !void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                const fd_safety = ref.scalar.analyte.fd_safety orelse return;
                const fd = getTracked(fd_safety.ref);
                if (fd.closed != null) return;
                if (returned_fds.contains(fd_safety.ref)) return;

                return reportFdLeak(ctx, fd.opened);
            },
            .errorunion => try checkFdLeakRecursive(refinements, ref.errorunion.to, ctx, returned_fds),
            .optional => try checkFdLeakRecursive(refinements, ref.optional.to, ctx, returned_fds),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    try checkFdLeakRecursive(refinements, field_gid, ctx, returned_fds);
                }
            },
            else => {},
        }
    }

    /// Recursively collect FD identities from a refinement tree.
    fn collectReachableFds(refinements: *Refinements, gid: Gid, fds: *std.AutoHashMap(FdRef, void)) void {
        collectReachableFdsInner(refinements, gid, fds, 0);
    }

    fn collectReachableFdsInner(
        refinements: *Refinements,
        gid: Gid,
        fds: *std.AutoHashMap(FdRef, void),
        depth: usize,
    ) void {
        if (depth > 100) return;
        if (gid >= refinements.list.items.len) return;

        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                // Check for FD state
                if (ref.scalar.analyte.fd_safety) |fd| {
                    fds.put(fd.ref, {}) catch return;
                }
            },
            .pointer => |p| collectReachableFdsInner(refinements, p.to, fds, depth + 1),
            .optional => |o| collectReachableFdsInner(refinements, o.to, fds, depth + 1),
            .errorunion => |e| collectReachableFdsInner(refinements, e.to, fds, depth + 1),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    collectReachableFdsInner(refinements, field_gid, fds, depth + 1);
                }
            },
            .@"union" => |u| {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        collectReachableFdsInner(refinements, field_gid, fds, depth + 1);
                    }
                }
            },
            else => {},
        }
    }

    // =========================================================================
    // Aggregate Init Handler
    // =========================================================================

    /// Handle aggregate_init - copy fd_safety state from source elements to struct fields.
    pub fn aggregate_init(state: State, index: usize, params: tag.AggregateInit) !void {
        const result_gid = state.results[index].refinement orelse
            std.debug.panic("fd_safety.aggregate_init: result instruction {d} has no refinement", .{index});
        const result_ref = state.refinements.at(result_gid);

        switch (result_ref.*) {
            .@"struct" => |s| {
                // For structs: copy fd_safety from each source element to corresponding field
                for (s.fields, 0..) |field_gid, i| {
                    if (i >= params.elements.len) break;
                    const src = params.elements[i];
                    copyFdSafetyState(state, field_gid, src);
                }
            },
            else => {},
        }
    }

    /// Copy fd_safety state from a source to a destination refinement.
    fn copyFdSafetyState(state: State, dst_gid: Gid, src: tag.Src) void {
        const src_gid: ?Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement orelse
                std.debug.panic("fd_safety.copyFdSafetyState: source instruction {d} has no refinement", .{inst}),
            .interned => null, // Interned values don't have fd_safety
            .fnptr => null, // Function pointers don't have fd_safety
        };

        // For interned/fnptr sources, nothing to copy
        if (src_gid == null) return;

        const src_ref = state.refinements.at(src_gid.?);
        const dst_ref = state.refinements.at(dst_gid);

        // Copy fd_safety for scalar types
        switch (dst_ref.*) {
            .scalar => |*s| {
                if (src_ref.* != .scalar) return;
                const src_fd = src_ref.scalar.analyte.fd_safety orelse return;
                s.analyte.fd_safety = src_fd;
            },
            .@"struct" => |s| {
                // If destination is a struct, recurse into fields
                if (src_ref.* != .@"struct") return;
                const src_s = src_ref.@"struct";
                for (s.fields, 0..) |field_gid, i| {
                    if (i < src_s.fields.len) {
                        copyFdSafetyStateRecursive(state.refinements, field_gid, src_s.fields[i]);
                    }
                }
            },
            .optional => |o| {
                if (src_ref.* != .optional) return;
                copyFdSafetyStateRecursive(state.refinements, o.to, src_ref.optional.to);
            },
            .errorunion => |e| {
                if (src_ref.* != .errorunion) return;
                copyFdSafetyStateRecursive(state.refinements, e.to, src_ref.errorunion.to);
            },
            // Other types don't have fd_safety
            else => {},
        }
    }

    /// Recursively copy fd_safety state from source GID to destination GID.
    fn copyFdSafetyStateRecursive(refinements: *Refinements, dst_gid: Gid, src_gid: Gid) void {
        const src_ref = refinements.at(src_gid);
        const dst_ref = refinements.at(dst_gid);

        switch (dst_ref.*) {
            .scalar => |*s| {
                if (src_ref.* != .scalar) return;
                const src_fd = src_ref.scalar.analyte.fd_safety orelse return;
                s.analyte.fd_safety = src_fd;
            },
            .@"struct" => |s| {
                if (src_ref.* != .@"struct") return;
                const src_s = src_ref.@"struct";
                for (s.fields, 0..) |field_gid, i| {
                    if (i < src_s.fields.len) {
                        copyFdSafetyStateRecursive(refinements, field_gid, src_s.fields[i]);
                    }
                }
            },
            .optional => |o| {
                if (src_ref.* != .optional) return;
                copyFdSafetyStateRecursive(refinements, o.to, src_ref.optional.to);
            },
            .errorunion => |e| {
                if (src_ref.* != .errorunion) return;
                copyFdSafetyStateRecursive(refinements, e.to, src_ref.errorunion.to);
            },
            else => {},
        }
    }

    pub fn bit_and(state: State, index: usize, params: tag.BinOp(.bit_and)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn bit_or(state: State, index: usize, params: tag.BinOp(.bit_or)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn xor(state: State, index: usize, params: tag.BinOp(.xor)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn min(state: State, index: usize, params: tag.BinOp(.min)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn max(state: State, index: usize, params: tag.BinOp(.max)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn add(state: State, index: usize, params: tag.BinOp(.add)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn sub(state: State, index: usize, params: tag.BinOp(.sub)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn mul(state: State, index: usize, params: tag.BinOp(.mul)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn div(state: State, index: usize, params: tag.BinOp(.div)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn mod(state: State, index: usize, params: tag.BinOp(.mod)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn rem(state: State, index: usize, params: tag.BinOp(.rem)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn shl(state: State, index: usize, params: tag.BinOp(.shl)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn shr(state: State, index: usize, params: tag.BinOp(.shr)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn add_with_overflow(state: State, index: usize, params: tag.OverflowOp(.add_with_overflow)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn sub_with_overflow(state: State, index: usize, params: tag.OverflowOp(.sub_with_overflow)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn mul_with_overflow(state: State, index: usize, params: tag.OverflowOp(.mul_with_overflow)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }
    pub fn shl_with_overflow(state: State, index: usize, params: tag.OverflowOp(.shl_with_overflow)) !void {
        try checkBinMath(state, index, params.lhs, params.rhs);
    }

    pub fn not(state: State, index: usize, params: tag.UnOp(.not)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn trunc(state: State, index: usize, params: tag.UnOp(.trunc)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn ctz(state: State, index: usize, params: tag.UnOp(.ctz)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn sqrt(state: State, index: usize, params: tag.UnOp(.sqrt)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn sin(state: State, index: usize, params: tag.UnOp(.sin)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn cos(state: State, index: usize, params: tag.UnOp(.cos)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn tan(state: State, index: usize, params: tag.UnOp(.tan)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn exp(state: State, index: usize, params: tag.UnOp(.exp)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn exp2(state: State, index: usize, params: tag.UnOp(.exp2)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn log(state: State, index: usize, params: tag.UnOp(.log)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn log2(state: State, index: usize, params: tag.UnOp(.log2)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn log10(state: State, index: usize, params: tag.UnOp(.log10)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn floor(state: State, index: usize, params: tag.UnOp(.floor)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn ceil(state: State, index: usize, params: tag.UnOp(.ceil)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn round(state: State, index: usize, params: tag.UnOp(.round)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn trunc_float(state: State, index: usize, params: tag.UnOp(.trunc_float)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn neg(state: State, index: usize, params: tag.UnOp(.neg)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn abs(state: State, index: usize, params: tag.UnOp(.abs)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn popcount(state: State, index: usize, params: tag.UnOp(.popcount)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn byte_swap(state: State, index: usize, params: tag.UnOp(.byte_swap)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn bit_reverse(state: State, index: usize, params: tag.UnOp(.bit_reverse)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn clz(state: State, index: usize, params: tag.UnOp(.clz)) !void {
        try checkUnaryMath(state, index, params.src);
    }
    pub fn reduce(state: State, index: usize, params: tag.Reduce) !void {
        try checkUnaryMath(state, index, params.src);
    }

    fn checkBinMath(state: State, index: usize, lhs: tag.Src, rhs: tag.Src) !void {
        try checkFdMathSrc(state, index, lhs);
        try checkFdMathSrc(state, index, rhs);
    }

    fn checkUnaryMath(state: State, index: usize, src: tag.Src) !void {
        try checkFdMathSrc(state, index, src);
    }

    fn checkFdMathSrc(state: State, index: usize, src: tag.Src) !void {
        _ = index;
        const gid: Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement orelse return,
            .interned, .fnptr => return,
        };
        const ref = state.refinements.at(gid);
        if (ref.* != .scalar) return;
        if (ref.scalar.analyte.fd_safety == null) return;
        return reportFdMath(state.ctx);
    }

    // =========================================================================
    // Error Reporting
    // =========================================================================

    fn reportDoubleClose(ctx: *Context, fd_state: Open, prev_close: Closed) anyerror {
        try ctx.meta.print(ctx.writer, "double close in ", .{});
        try prev_close.meta.print(ctx.writer, "previously closed in ", .{});
        try fd_state.meta.print(ctx.writer, "originally opened in ", .{});
        return error.DoubleClose;
    }

    fn reportUseAfterClose(ctx: *Context, fd_state: Open, close_site: Closed) anyerror {
        try ctx.meta.print(ctx.writer, "use after close in ", .{});
        try close_site.meta.print(ctx.writer, "closed in ", .{});
        try fd_state.meta.print(ctx.writer, "opened in ", .{});
        return error.UseAfterClose;
    }

    fn reportFdLeak(ctx: *Context, fd_state: Open) anyerror {
        try ctx.meta.print(ctx.writer, "fd leak in ", .{});
        try fd_state.meta.print(ctx.writer, "opened in ", .{});
        return error.FdLeak;
    }

    fn reportFdMath(ctx: *Context) anyerror {
        try ctx.meta.print(ctx.writer, "fd used in mathematical operation in ", .{});
        return error.FdMath;
    }
};

fn createTracked(open: Open) !FdSafety {
    if (!initialized) @panic("FdSafety module not initialized");
    const ref = tracked.items.len;
    try tracked.append(list_allocator, .{ .opened = open });
    return .{ .ref = ref };
}

fn getTracked(ref: FdRef) *TrackedFd {
    return getTrackedOrNull(ref) orelse @panic("invalid fd_safety ref");
}

fn getTrackedOrNull(ref: FdRef) ?*TrackedFd {
    if (!initialized) return null;
    if (ref >= tracked.items.len) return null;
    return &tracked.items[ref];
}

const debug = @import("builtin").mode == .Debug;

/// Validates that fd_safety is correctly set on refinements.
/// - ALLOWED: .scalar (file descriptors are integer values)
/// - MUST BE NULL: .pointer, .optional, .errorunion, .struct, .union, .recursive, .fnptr, .allocator
/// - NO ANALYTE: .void, .noreturn, .unimplemented
pub fn testValid(refinement: Refinements.Refinement, idx: usize) void {
    if (!debug) return;
    switch (refinement) {
        // ALLOWED - fd_safety can be null or non-null on scalars
        .scalar => {},
        // MUST BE NULL - fd_safety only applies to scalar file descriptors
        .pointer => |p| {
            if (p.analyte.fd_safety != null) std.debug.panic("fd_safety must be null on pointer (idx={d})", .{idx});
        },
        .optional => |o| {
            if (o.analyte.fd_safety != null) std.debug.panic("fd_safety must be null on optional (idx={d})", .{idx});
        },
        .errorunion => |e| {
            if (e.analyte.fd_safety != null) std.debug.panic("fd_safety must be null on errorunion (idx={d})", .{idx});
        },
        .@"struct" => |st| {
            if (st.analyte.fd_safety != null) std.debug.panic("fd_safety must be null on struct (idx={d})", .{idx});
        },
        .@"union" => |u| {
            if (u.analyte.fd_safety != null) std.debug.panic("fd_safety must be null on union (idx={d})", .{idx});
        },
        .recursive => |rec| {
            if (rec.analyte.fd_safety != null) std.debug.panic("fd_safety must be null on recursive (idx={d})", .{idx});
        },
        .fnptr => |f| {
            if (f.analyte.fd_safety != null) std.debug.panic("fd_safety must be null on fnptr (idx={d})", .{idx});
        },
        .allocator => |a| {
            if (a.analyte.fd_safety != null) std.debug.panic("fd_safety must be null on allocator (idx={d})", .{idx});
        },
        // NO ANALYTE - trivial types
        .void, .noreturn, .unimplemented => {},
    }
}
